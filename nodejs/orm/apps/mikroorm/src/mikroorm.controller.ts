import { Controller, Get } from '@nestjs/common';
import { MikroormService } from './mikroorm.service';

@Controller()
export class MikroormController {
  constructor(private readonly mikroormService: MikroormService) {}

  @Get()
  getHello(): string {
    return this.mikroormService.getHello();
  }
}
