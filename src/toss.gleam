import gleam/result

/// The Erlang type for FFI
type Socket

/// Address of a peer
pub type Address {
  Hostname(String)
  Ipv4(Int, Int, Int, Int)
  Ipv6(Int, Int, Int, Int, Int, Int, Int, Int)
}

pub opaque type SinglePeerSocket {
  SinglePeerSocket(socket: Socket)
}

pub opaque type MultiPeerSocket {
  MultiPeerSocket(socket: Socket)
}

// TODO: Options?
/// Opens a new UDP socket connected to the specified peer.
/// Datagrams can only be sent to,
/// and will only be received from the specified peer.
/// The socket and the local port number are returned.
/// 0 can be used for the local port to let the OS automatically
/// choose a free port.
pub fn connect(
  local_port local_port: Int,
  host host: Address,
  remote_port port: Int,
) -> Result(SinglePeerSocket, Error) {
  use socket <- result.try(open_socket(local_port))
  use _ <- result.map(connect_socket(socket, host, port))
  SinglePeerSocket(socket)
}

/// Returns the local port, if the socket was opened with port 0
pub fn local_port(socket: SinglePeerSocket) -> Result(Int, Nil) {
  inet_port(socket.socket) |> result.replace_error(Nil)
}

/// Sends data to the peer of a connected socket
pub fn send(socket: SinglePeerSocket, data: BitArray) -> Result(Nil, Error) {
  send_connected(socket.socket, data)
}

// TODO: Options?
/// Opens a socket that can receive from and send to many peers.
pub fn open(local_port: Int) -> Result(MultiPeerSocket, Error) {
  use socket <- result.map(open_socket(local_port))
  MultiPeerSocket(socket)
}

// TODO: different address schemes?
/// Sends a UDP datagram to the specified destination on a multi-peer socket.
pub fn send_to(
  socket: MultiPeerSocket,
  address: Address,
  port: Int,
  data: BitArray,
) -> Result(Nil, Error) {
  send_unconnected(socket.socket, address, port, data)
}

/// Receives a UDP datagram from the connected peer.
pub fn receive(
  socket: SinglePeerSocket,
  max_length: Int,
  timeout: Int,
) -> Result(BitArray, Error) {
  use #(_, _, data) <- result.map(recv(socket.socket, max_length, timeout))
  data
}

/// Receives a UDP datagram from any source on a multi peer socket.
/// The source address, port, and the datagram are returned on success.
/// The maximum length will affect memory allocation,
/// so it should be selected conservatively.
pub fn receive_any(
  socket: MultiPeerSocket,
  max_length max_length: Int,
  timeout_milliseconds timeout: Int,
) -> Result(#(Address, Int, BitArray), Error) {
  recv(socket.socket, max_length, timeout)
}

// TODO: Add message based API

fn open_socket(port: Int) -> Result(Socket, Error) {
  gen_udp_open(port, [Mode(Binary), Active(passive())])
}

/// any() from Erlang
type Any

type ModeValue {
  Binary
}

type ActiveValue

type GenUdpOption {
  Active(ActiveValue)
  Mode(ModeValue)
}

@external(erlang, "inet", "port")
fn inet_port(socket: Socket) -> Result(Int, Any)

@external(erlang, "gen_udp", "open")
fn gen_udp_open(port: Int, opts: List(GenUdpOption)) -> Result(Socket, Error)

@external(erlang, "toss_ffi", "recv")
fn recv(
  socket: Socket,
  max_length: Int,
  timeout: Int,
) -> Result(#(Address, Int, BitArray), Error)

@external(erlang, "toss_ffi", "connect")
fn connect_socket(
  socket: Socket,
  host: Address,
  port: Int,
) -> Result(Nil, Error)

@external(erlang, "toss_ffi", "send")
fn send_connected(socket: Socket, data: BitArray) -> Result(Nil, Error)

@external(erlang, "toss_ffi", "send")
fn send_unconnected(
  socket: Socket,
  host: Address,
  port: Int,
  data: BitArray,
) -> Result(Nil, Error)

@external(erlang, "toss_ffi", "passive")
fn passive() -> ActiveValue

@external(erlang, "toss_ffi", "active_once")
fn active_once() -> ActiveValue

// Everything below is copied from mug by Louis Pilfold, Licensed under Apache-2.0
// https://github.com/lpil/mug

/// Errors that can occur when working with UDP sockets.
///
/// For more information on these errors see the Erlang documentation:
/// - https://www.erlang.org/doc/man/file#type-posix
/// - https://www.erlang.org/doc/man/inet#type-posix
///
pub type Error {
  // https://www.erlang.org/doc/man/inet#type-posix
  /// Connection closed
  Closed
  /// Operation timed out
  Timeout
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
    Closed -> "Connection closed"
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
