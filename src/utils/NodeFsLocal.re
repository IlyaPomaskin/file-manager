module Stats = {
  type t;
  type dev = int;
  type ino = int;
  type mode = int;
  type nlink = int;
  type uid = int;
  type gid = int;
  type rdev = int;
  type size = int;
  type blksize = int;
  type blocks = int;
  type atimeMs = int;
  type mtimeMs = int;
  type ctimeMs = int;
  type birthtimeMs = int;
  type atime = Js.Date.t;
  type mtime = Js.Date.t;
  type ctime = Js.Date.t;
  type birthtime = Js.Date.t;
  [@bs.send] external isFile : t => bool = "isFile";
  [@bs.send] external isDirectory : t => bool = "isDirectory";
  [@bs.send] external isBlockDevice : t => bool = "isBlockDevice";
  [@bs.send] external isCharacterDevice : t => bool = "isCharacterDevice";
  [@bs.send] external isFIFO : t => bool = "isFIFO";
  [@bs.send] external isSocket : t => bool = "isSocket";
};

[@bs.val] [@bs.module "fs"] external statSync : string => Stats.t = "statSync";

[@bs.val] [@bs.module "os"] external homedir : unit => string = "homedir";