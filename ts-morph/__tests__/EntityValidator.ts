import { ClassDeclaration, Project, Node, SourceFile } from 'ts-morph';

export class EntityValidator {
  #project: Project;

  constructor(path: string, content?: string) {
    this.#project = new Project();

    if (content) {
      this.#project.createSourceFile(path, content);
      return;
    }

    this.#project.addSourceFilesAtPaths(path);
  }

  validate(): void {
    const sourceFiles = this.#project.getSourceFiles();

    sourceFiles.forEach((sourceFile) => {
      const entityClass = this.getClass(sourceFile);
      const joinColumns = this.getJoinColumns(entityClass);
      const indexColumns = this.getIndexColumns(entityClass, joinColumns);

      if (indexColumns.length !== joinColumns.length) {
        throw new Error(
          `JoinColumn 에 매칭되는 Index 선언이 누락되었습니다: ${JSON.stringify(
            {
              entity: entityClass.getName(),
              joinColumns,
              indexColumns,
            },
            null,
            2,
          )}`,
        );
      }
    });
  }

  private getClass(sourceFile: SourceFile): ClassDeclaration {
    const entityClass = sourceFile.getClass((declaration) =>
      declaration
        .getDecorators()
        .some((decorator) => decorator.getFullName() === 'Entity'),
    );

    if (!entityClass) {
      throw new Error('Entity 데코레이터가 존재하지 않습니다');
    }

    return entityClass;
  }

  private getJoinColumns(entityClass: ClassDeclaration): string[] {
    return entityClass
      .getProperties()
      .filter((property) =>
        property
          .getDecorators()
          .some((decorator) => decorator.getFullName() === 'JoinColumn'),
      )
      .map((property) => property.getName());
  }

  private getIndexColumns(
    entityClass: ClassDeclaration,
    joinColumns: string[],
  ): string[] {
    return entityClass
      .getDecorators()
      .filter((decorator) => decorator.getFullName() === 'Index')
      .map((decorator) => decorator.getArguments())
      .map(([_, indexField]) => this.parseArgument(indexField))
      .filter((columnName) => joinColumns.includes(columnName));
  }

  private parseArgument(node?: Node): string {
    if (!node) {
      return '';
    }

    return node.getFullText().replace(/[\W\s]/gi, '');
  }
}
