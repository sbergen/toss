//// Work with UDP sockets on the Erlang target.

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/result

/// A UDP socket, used to send and receive UDP datagrams.
pub type Socket

/// A handle to a socket,
/// which can be used to send datagrams without specifying a destination.
/// This type works as a token of proof that
/// [`connect`](#connect) has been successfully called _at least once_.
/// It is still tied to the underlying socket, and is not unique.
pub type ConnectedSender

/// IP address of a peer
pub type IpAddress {
  Ipv4Address(Int, Int, Int, Int)
  Ipv6Address(Int, Int, Int, Int, Int, Int, Int, Int)
}

/// Converts an IP address to a string.
@external(erlang, "toss_ffi", "ip_to_string")
pub fn ip_to_string(ip: IpAddress) -> String

/// Tries to parse an IP address from a string.
@external(erlang, "toss_ffi", "parse_ip")
pub fn prase_ip(address: String) -> Result(IpAddress, Nil)

/// The set of options used to open a socket.
/// (Mostly serves forward compatibility purposes at the moment,
/// as only the port and IP version can be specified.)
pub opaque type SocketOptions {
  SocketOptions(local_port: Int, options: List(GenUdpOption))
}

/// Constructs the default options to open a socket with,
/// binding it to the given local port.
/// 0 can be used to let the OS automatically choose a free port.
pub fn new(port port: Int) -> SocketOptions {
  SocketOptions(port, [Mode(Binary), Active(passive())])
}

/// Specifies to open the socket in IPv4 mode.
/// You can not send to IPv6 addresses when opening a socket with this option.
pub fn use_ipv4(options: SocketOptions) -> SocketOptions {
  add_option(options, Inet)
}

/// Specifies to open the socket in IPv6 mode.
/// You can not send to IPv4 addresses when opening a socket with this option.
pub fn use_ipv6(options: SocketOptions) -> SocketOptions {
  add_option(options, Inet6)
}

fn add_option(options: SocketOptions, option: GenUdpOption) -> SocketOptions {
  SocketOptions(..options, options: [option, ..options.options])
}

/// Opens a UDP socket.
/// When freshly opened, the socket will receive data from any source.
pub fn open(options: SocketOptions) -> Result(Socket, Error) {
  gen_udp_open(options.local_port, options.options)
}

/// Closes the socket, freeing up any resources it uses.
/// The socket and any associated senders can no longer be used after this.
pub fn close(socket: Socket) -> Nil {
  gen_udp_close(socket)
  Nil
}

/// Returns the local port, useful if the socket was opened with port 0
pub fn local_port(socket: Socket) -> Result(Int, Nil) {
  inet_port(socket) |> result.replace_error(Nil)
}

/// Sends a UDP datagram to the specified destination by IP address.
@external(erlang, "toss_ffi", "send")
pub fn send_to(
  socket: Socket,
  host: IpAddress,
  port: Int,
  data: BitArray,
) -> Result(Nil, Error)

/// Sends a UDP datagram to the specified destination by hostname.
@external(erlang, "toss_ffi", "send")
pub fn send_to_host(
  socket: Socket,
  host: String,
  port: Int,
  data: BitArray,
) -> Result(Nil, Error)

/// Receives a UDP datagram from the socket.
/// The source address, port, and the datagram are returned on success.
/// The maximum length will affect memory allocation,
/// so it should be selected conservatively.
/// The address will be an error if the sender does not have a socket address,
/// or the Erlang VM
/// [doesn't recognise the address](https://www.erlang.org/doc/apps/kernel/inet#t:returned_non_ip_address/0)
/// (toss does currently not support local Unix domain sockets).
@external(erlang, "toss_ffi", "recv")
pub fn receive(
  socket: Socket,
  max_length max_length: Int,
  timeout_milliseconds timeout: Int,
) -> Result(#(Result(IpAddress, Nil), Int, BitArray), Error)

/// Receives a UDP datagram from the socket, without a timeout.
/// See [`receive`](#receive) for details.
@external(erlang, "toss_ffi", "recv")
pub fn receive_forever(
  socket: Socket,
  max_length max_length: Int,
) -> Result(#(Result(IpAddress, Nil), Int, BitArray), Error)

/// Modifies the socket to only receive data from the specified source.
/// Other messages are discarded on arrival by the OS protocol stack.
/// Returns a handle to the socket,
/// which can be used to send data without specifying the destination every time.
/// Note that multiple calls to `connect_to` will override any previous calls -
/// all previously returned senders will also change behaviour.
@external(erlang, "toss_ffi", "connect")
pub fn connect_to(
  socket: Socket,
  host: IpAddress,
  port port: Int,
) -> Result(ConnectedSender, Error)

/// Like `connect_to`, but uses a host name instead of an IP address.
@external(erlang, "toss_ffi", "connect")
pub fn connect_to_host(
  socket: Socket,
  host: String,
  port port: Int,
) -> Result(ConnectedSender, Error)

/// Sends a UDP datagram to the peer of a connected socket.
@external(erlang, "toss_ffi", "send")
pub fn send(sender: ConnectedSender, data: BitArray) -> Result(Nil, Error)

/// Messages that can be sent by the socket to the process that controls it.
pub type UdpMessage {
  /// An incoming UDP datagram
  Datagram(
    socket: Socket,
    host: Result(IpAddress, Nil),
    peer_port: Int,
    data: BitArray,
  )
  UdpError(Socket, Error)
}

/// Configure a selector to receive messages from UDP sockets.
/// You will also need to call
/// [`receive_next_datagram_as_message`](#receive_next_datagram_as_message)
/// to use the selector successfully - once initially,
/// and again after receiving each message.
///
/// Note that this will receive messages from all UDP sockets that the process controls,
/// rather than any specific one.
/// If you wish to only handle messages from one socket then use one process per socket.
pub fn select_udp_messages(
  selector: process.Selector(a),
  mapper: fn(UdpMessage) -> a,
) -> process.Selector(a) {
  let udp = atom.create("udp")
  let error = atom.create("udp_error")

  let map = fn(message) { mapper(unsafe_decode(message)) }

  selector
  |> process.select_record(udp, 4, map)
  |> process.select_record(error, 2, map)
}

/// Switch the socket to active (once) mode,
/// meaning that the next datagram received on the socket
/// will be sent as an Erlang message to the socket owner's inbox.
///
/// This is useful for when you wish to have an OTP actor handle incoming messages,
/// as using the [`receive`](#receive) function would result in the actor being
/// blocked and unable to handle other messages while waiting for the next packet.
///
/// Messages will be sent to the process that controls the socket,
/// which is the process that established the socket with the [`open`](#open) function.
///
/// In order to continue receiving messages,
/// you will need to call this function again after receiving a message.
/// This is intended to provide backpressure to the OS socket,
/// instead of flooding the inbox on the Erlang side,
/// which could happen if switching to full active mode.
pub fn receive_next_datagram_as_message(socket: Socket) -> Nil {
  set_socket_options(socket, [Active(active_once())])
  Nil
}

/// any() from Erlang, or I don't care about the return value
type Any

type ModeValue {
  Binary
}

type ActiveValue

type GenUdpOption {
  Active(ActiveValue)
  Mode(ModeValue)
  Inet
  Inet6
}

@external(erlang, "inet", "setopts")
fn set_socket_options(socket: Socket, options: List(GenUdpOption)) -> Any

@external(erlang, "inet", "port")
fn inet_port(socket: Socket) -> Result(Int, Any)

@external(erlang, "gen_udp", "open")
fn gen_udp_open(port: Int, opts: List(GenUdpOption)) -> Result(Socket, Error)

@external(erlang, "gen_udp", "close")
fn gen_udp_close(socket: Socket) -> Any

@external(erlang, "toss_ffi", "passive")
fn passive() -> ActiveValue

@external(erlang, "toss_ffi", "active_once")
fn active_once() -> ActiveValue

@external(erlang, "toss_ffi", "map_udp_message")
fn unsafe_decode(message: Dynamic) -> UdpMessage

// Everything below is copied from mug by Louis Pilfold, Licensed under Apache-2.0,
// with only a few error variants and documentation changes to work with UDP instead of TCP.
// https://github.com/lpil/mug

/// Errors that can occur when working with UDP sockets.
///
/// For more information on these errors see the
/// [Erlang documentation](https://www.erlang.org/doc/apps/kernel/inet#t:posix/0).
pub type Error {
  /// Socket not owned by the process trying to use it.
  /// This is documented as an error value in the
  /// [`gen_udp` documentation](https://www.erlang.org/doc/apps/kernel/gen_udp.html),
  /// but it's unclear how to trigger it.
  NotOwner
  /// Operation timed out
  Timeout
  // https://www.erlang.org/doc/maninet#type-posix
  /// Address already in use
  Eaddrinuse
  /// Cannot assign requested address
  Eaddrnotavail
  /// Address family not supported
  Eafnosupport
  /// Operation already in progress
  Ealready
  /// Connection aborted
  Econnaborted
  /// Connection refused
  Econnrefused
  /// Connection reset by peer
  Econnreset
  /// Destination address required
  Edestaddrreq
  /// Host is down
  Ehostdown
  /// No route to host
  Ehostunreach
  /// Operation now in progress
  Einprogress
  /// Socket is already connected
  Eisconn
  /// Message too long
  Emsgsize
  /// Network is down
  Enetdown
  /// Network is unreachable
  Enetunreach
  /// Package not installed
  Enopkg
  /// Protocol not available
  Enoprotoopt
  /// Socket is not connected
  Enotconn
  /// Inappropriate ioctl for device
  Enotty
  /// Socket operation on non-socket
  Enotsock
  /// Protocol error
  Eproto
  /// Protocol not supported
  Eprotonosupport
  /// Protocol wrong type for socket
  Eprototype
  /// Socket type not supported
  Esocktnosupport
  /// Connection timed out
  Etimedout
  /// Operation would block
  Ewouldblock
  /// Bad port number
  Exbadport
  /// Bad sequence number
  Exbadseq
  /// Non-existent domain
  Nxdomain

  // https://www.erlang.org/doc/man/file#type-posix
  /// Permission denied
  Eacces
  /// Resource temporarily unavailable
  Eagain
  /// Bad file descriptor
  Ebadf
  /// Bad message
  Ebadmsg
  /// Device or resource busy
  Ebusy
  /// Resource deadlock avoided
  Edeadlk
  /// Resource deadlock avoided
  Edeadlock
  /// Disk quota exceeded
  Edquot
  /// File exists
  Eexist
  /// Bad address
  Efault
  /// File too large
  Efbig
  /// Inappropriate file type or format
  Eftype
  /// Interrupted system call
  Eintr
  /// Invalid argument
  Einval
  /// Input/output error
  Eio
  /// Is a directory
  Eisdir
  /// Too many levels of symbolic links
  Eloop
  /// Too many open files
  Emfile
  /// Too many links
  Emlink
  /// Multihop attempted
  Emultihop
  /// File name too long
  Enametoolong
  /// Too many open files in system
  Enfile
  /// No buffer space available
  Enobufs
  /// No such device
  Enodev
  /// No locks available
  Enolck
  /// Link has been severed
  Enolink
  /// No such file or directory
  Enoent
  /// Out of memory
  Enomem
  /// No space left on device
  Enospc
  /// Out of streams resources
  Enosr
  /// Device not a stream
  Enostr
  /// Function not implemented
  Enosys
  /// Block device required
  Enotblk
  /// Not a directory
  Enotdir
  /// Operation not supported
  Enotsup
  /// No such device or address
  Enxio
  /// Operation not supported on socket
  Eopnotsupp
  /// Value too large for defined data type
  Eoverflow
  /// Operation not permitted
  Eperm
  /// Broken pipe
  Epipe
  /// Result too large
  Erange
  /// Read-only file system
  Erofs
  /// Illegal seek
  Espipe
  /// No such process
  Esrch
  /// Stale file handle
  Estale
  /// Text file busy
  Etxtbsy
  /// Cross-device link
  Exdev
}

/// Convert an error into a human-readable description
///
pub fn describe_error(error: Error) -> String {
  case error {
    NotOwner -> "Socket not owned by the process trying to use it"
    Timeout -> "Operation timed out"
    Eaddrinuse -> "Address already in use"
    Eaddrnotavail -> "Cannot assign requested address"
    Eafnosupport -> "Address family not supported"
    Ealready -> "Operation already in progress"
    Econnaborted -> "Connection aborted"
    Econnrefused -> "Connection refused"
    Econnreset -> "Connection reset by peer"
    Edestaddrreq -> "Destination address required"
    Ehostdown -> "Host is down"
    Ehostunreach -> "No route to host"
    Einprogress -> "Operation now in progress"
    Eisconn -> "Socket is already connected"
    Emsgsize -> "Message too long"
    Enetdown -> "Network is down"
    Enetunreach -> "Network is unreachable"
    Enopkg -> "Package not installed"
    Enoprotoopt -> "Protocol not available"
    Enotconn -> "Socket is not connected"
    Enotty -> "Inappropriate ioctl for device"
    Enotsock -> "Socket operation on non-socket"
    Eproto -> "Protocol error"
    Eprotonosupport -> "Protocol not supported"
    Eprototype -> "Protocol wrong type for socket"
    Esocktnosupport -> "Socket type not supported"
    Etimedout -> "Connection timed out"
    Ewouldblock -> "Operation would block"
    Exbadport -> "Bad port number"
    Exbadseq -> "Bad sequence number"
    Nxdomain -> "Non-existent domain"
    Eacces -> "Permission denied"
    Eagain -> "Resource temporarily unavailable"
    Ebadf -> "Bad file descriptor"
    Ebadmsg -> "Bad message"
    Ebusy -> "Device or resource busy"
    Edeadlk -> "Resource deadlock avoided"
    Edeadlock -> "Resource deadlock avoided"
    Edquot -> "Disk quota exceeded"
    Eexist -> "File exists"
    Efault -> "Bad address"
    Efbig -> "File too large"
    Eftype -> "Inappropriate file type or format"
    Eintr -> "Interrupted system call"
    Einval -> "Invalid argument"
    Eio -> "Input/output error"
    Eisdir -> "Is a directory"
    Eloop -> "Too many levels of symbolic links"
    Emfile -> "Too many open files"
    Emlink -> "Too many links"
    Emultihop -> "Multihop attempted"
    Enametoolong -> "File name too long"
    Enfile -> "Too many open files in system"
    Enobufs -> "No buffer space available"
    Enodev -> "No such device"
    Enolck -> "No locks available"
    Enolink -> "Link has been severed"
    Enoent -> "No such file or directory"
    Enomem -> "Out of memory"
    Enospc -> "No space left on device"
    Enosr -> "Out of streams resources"
    Enostr -> "Device not a stream"
    Enosys -> "Function not implemented"
    Enotblk -> "Block device required"
    Enotdir -> "Not a directory"
    Enotsup -> "Operation not supported"
    Enxio -> "No such device or address"
    Eopnotsupp -> "Operation not supported on socket"
    Eoverflow -> "Value too large for defined data type"
    Eperm -> "Operation not permitted"
    Epipe -> "Broken pipe"
    Erange -> "Result too large"
    Erofs -> "Read-only file system"
    Espipe -> "Illegal seek"
    Esrch -> "No such process"
    Estale -> "Stale file handle"
    Etxtbsy -> "Text file busy"
    Exdev -> "Cross-device link"
  }
}
