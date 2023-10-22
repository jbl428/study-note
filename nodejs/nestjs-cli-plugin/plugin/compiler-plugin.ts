import type ts from 'typescript';

export const before: (
  options: Record<string, any> | undefined,
  program: ts.Program,
) => ts.TransformerFactory<ts.SourceFile> | ts.CustomTransformerFactory = (
  options,
  program,
) => {
  console.log('options', options);

  return (ctx) => {
    return (sourceFile) => {
      console.log('sourceFile', sourceFile.fileName);

      return sourceFile;
    };
  };
};
