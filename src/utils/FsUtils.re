open Types;

open Types.FileInfo;

let make = (path, name) : FileInfo.t => {
  let fullPath = Node_path.resolve(path, name);
  let stats = NodeFsLocal.statSync(fullPath);
  {name, fullPath, isFile: NodeFsLocal.Stats.isFile(stats)};
};

let makeParent = path : FileInfo.t => {
  name: "..",
  fullPath: path,
  isFile: false
};

let getDirPath = (oldPath, newPart) =>
  Node_path.normalize(oldPath ++ Node_path.sep ++ newPart);

let sortByTypeAndName = (a: FileInfo.t, b: FileInfo.t) =>
  switch (a.isFile, b.isFile) {
  | (false, false)
  | (true, true) => String.compare(a.name, b.name)
  | (false, true) => (-1)
  | (true, false) => 1
  };

let getFilesList = path : list(FileInfo.t) =>
  Node_fs.readdirSync(path)
  |> Array.to_list
  |> List.map(filename => make(path, filename))
  |> (
    list =>
      switch path {
      | "/" => list
      | _ => List.append([makeParent(path)], list)
      }
  )
  |> List.fast_sort(sortByTypeAndName);