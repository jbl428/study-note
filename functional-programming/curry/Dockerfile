FROM node:current-alpine

RUN yarn init -y && yarn add ts-node typescript

ADD index.ts ./

ENTRYPOINT ["yarn", "ts-node", "index.ts"]