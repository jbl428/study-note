import { MikroOrmModuleOptions } from '@mikro-orm/nestjs';
import { TsMorphMetadataProvider } from '@mikro-orm/reflection';
import { TSMigrationGenerator } from '@mikro-orm/migrations';

class CustomMigrationGenerator extends TSMigrationGenerator {
  createStatement(sql: string, padLeft: number): string {
    if (sql.includes(' add constraint ') || sql.includes(' drop constraint '))
      return '';

    return super.createStatement(sql, padLeft);
  }
}

const config: MikroOrmModuleOptions = {
  type: 'postgresql',
  user: 'test',
  password: 'test',
  dbName: 'test',
  port: 5432,
  metadataProvider: TsMorphMetadataProvider,
  autoLoadEntities: true,
  schemaGenerator: {
    createForeignKeyConstraints: false,
  },
  migrations: {
    disableForeignKeys: true,
    path: 'migrations',
    pathTs: 'migrations',
    generator: CustomMigrationGenerator,
  },
};

export default config;
