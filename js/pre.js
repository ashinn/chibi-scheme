Module['preRun'].push(function () {
  FS.writeFile('program.scm', Module['program']);
});
Module['arguments'] = Module['arguments'] || [];
Module['arguments'].unshift('program.scm');

