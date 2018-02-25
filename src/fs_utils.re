module FileInfo = {
  type t = {
    name: string,
    fullPath: string,
    isFile: bool
  };
  let make = (path, name) : t => {
    let fullPath = Node_path.resolve(path, name);
    let stats = NodeFsLocal.statSync(fullPath);
    {name, fullPath, isFile: NodeFsLocal.Stats.isFile(stats)};
  };
  let makeParent = path : t => {name: "..", fullPath: path, isFile: false};
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
  |> List.map(filename => FileInfo.make(path, filename))
  |> List.append([FileInfo.makeParent(path)])
  |> List.fast_sort(sortByTypeAndName);