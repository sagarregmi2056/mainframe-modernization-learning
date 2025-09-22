# Use Node.js LTS version
FROM node:alpine

# Create app directory
WORKDIR /usr/src/app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install --only=production

# Copy app source
COPY . .

# Expose port
EXPOSE 4000

# Start command
CMD [ "node", "server.js" ]
