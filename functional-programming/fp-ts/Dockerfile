FROM node:current-alpine

RUN yarn install

COPY . .

ENTRYPOINT ["yarn", "ts-node", "index.ts"]
