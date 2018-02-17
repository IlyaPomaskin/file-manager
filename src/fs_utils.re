type fileInfo = {
  name: string,
  fullPath: string,
  isFile: bool
};

let getDirPath = (oldPath, newPart) =>
  Node_path.normalize(oldPath ++ Node_path.sep ++ newPart);

let mapToFileInfo = (path, name) : fileInfo => {
  let fullPath = Node_path.resolve(path, name);
  let stats = NodeFsLocal.statSync(fullPath);
  {name, fullPath, isFile: NodeFsLocal.Stats.isFile(stats)};
};

let sortByTypeAndName = (a: fileInfo, b: fileInfo) =>
  if (a.isFile === b.isFile) {
    String.compare(a.name, b.name);
  } else {
    switch (a, b) {
    | ({isFile: false}, {isFile: true}) => (-1)
    | ({isFile: true}, {isFile: false}) => 1
    | _ => 0
    };
  };

let getFilesList = path => {
  let files: array(fileInfo) =
    Node_fs.readdirSync(path)
    |> Array.map(filename => mapToFileInfo(path, filename))
    |> Array.append([|{name: "..", fullPath: path, isFile: false}|]);
  Array.fast_sort(sortByTypeAndName, files);
  Array.to_list(files);
};