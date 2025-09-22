# Use Node.js LTS version
FROM node:alpine

WORKDIR /usr/src/app

COPY package*.json ./

RUN npm install --only=production

COPY . .

EXPOSE 3002

CMD [ "node", "server.js" ]
