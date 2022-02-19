import { Injectable } from '@nestjs/common';

@Injectable()
export class TypeormService {
  getHello(): string {
    return 'Hello World!';
  }
}
