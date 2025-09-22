# Mainframe Modernization Learning Journey

## My Story and Challenges
When I started exploring mainframe modernization, I faced several real-world challenges that led to this project. The original IBM samples required the `ibm-zcon-server:3.0.96` Docker image, which wasn't publicly available. This is a common hurdle for many developers starting with mainframe development - access to the actual mainframe environment or tools isn't always straightforward.

Instead of giving up, I decided to turn this challenge into an opportunity. I created this mock environment to:
1. Understand how mainframe services work
2. Learn integration patterns
3. Practice modern API development
4. Simulate real-world scenarios

## Project Evolution
My journey evolved through several stages:

1. **Initial Challenge**:
   - Tried running IBM sample code
   - Discovered the Docker image wasn't accessible
   - Needed a way to learn without z/OS environment

2. **Solution Approach**:
   - Created mock services for each mainframe component
   - Built an API Gateway to simulate z/OS Connect
   - Implemented monitoring and logging
   - Added documentation and examples

3. **Next Steps**:
   - Dockerizing each service
   - Creating Docker Compose setup
   - Deploying to local Minikube
   - Adding more enterprise features

## Architecture and Components

### Current Setup
```
Client → API Gateway → [CICS Service (Catalog)]
                    → [DB2 Service  (Employee)]
                    → [IMS Service  (Phonebook)]
```

### Planned Container Architecture
```
Docker Compose:
├── CICS Service Container
├── DB2 Service Container
├── IMS Service Container
└── API Gateway Container
    └── Kubernetes Deployment (Minikube)
```

## Key Features
- REST API Gateway pattern
- Multiple backend service integration
- Data transformation examples
- Basic monitoring and logging


## Services

### 1. CICS Catalog Service
Demonstrates transaction processing with COMMAREA structures
- GET /catalog/items
- GET /catalog/items/{id}
- POST /catalog/orders

### 2. DB2 Employee Service
Shows relational database operations
- GET /employees
- GET /employees/{id}
- POST /employees
- PUT /employees/{id}
- DELETE /employees/{id}

### 3. IMS Phonebook Service
Illustrates hierarchical database operations
- POST /phonebook/contacts
- GET /phonebook/contacts/{lastName}
- PUT /phonebook/contacts/{lastName}
- DELETE /phonebook/contacts/{lastName}

## Learning Value
This project helped me understand:
- How different mainframe services interact
- Data transformation patterns
- API Gateway architecture
- Monitoring and logging importance
- Container-based deployment

## Getting Started

1. Install dependencies:
```bash
npm install
```

2. Start the services:
```bash


docker-compose up --build 

or,
# Start CICS Mock
cd ../simple-cics-mock-api
npm start

# Start DB2 Mock
cd ../simple-db2-mock-api
npm start

# Start IMS Mock
cd ../simple-ims-mock-api
npm start

# Start Gateway
cd ../mock-api-gateway
npm start
```

## Example Requests

### CICS Catalog API
```bash
# Get all items
curl http://localhost:3000/catalog/items

# Get specific item
curl http://localhost:3000/catalog/items/1001

# Place order
curl -X POST "http://localhost:3000/catalog/orders?itemNumber=1001&quantity=5"
```

### DB2 Employee API
```bash
# Get all employees
curl http://localhost:3000/employees

# Get employee by ID
curl http://localhost:3000/employees/000010

# Add new employee
curl -X POST http://localhost:3000/employees \
  -H "Content-Type: application/json" \
  -d '{
    "employeeNumber": "000030",
    "firstName": "JOHN",
    "lastName": "DOE",
    "department": "A00"
  }'
```

### IMS Phonebook API
```bash
# Get contact
curl http://localhost:3000/phonebook/contacts/Smith

# Add contact
curl -X POST http://localhost:3000/phonebook/contacts \
  -H "Content-Type: application/json" \
  -d '{
    "firstName": "John",
    "lastName": "Smith",
    "extension": "555-0101",
    "zipCode": "12345"
  }'



  # useful commands for monitoring and logging
  # View logs of all services
docker-compose logs

# View logs of specific service
docker-compose logs gateway

# Stop all services
docker-compose down

# Rebuild and restart a specific service
docker-compose up --build gateway



```



## Lessons Learned
1. **Real-world Challenges**: Not everything is immediately accessible (like the IBM Docker image), but there are always alternative ways to learn and practice.

2. **Integration Patterns**: Understanding how different services communicate is crucial, whether it's actual mainframe services or mock implementations.

3. **Modern Tools**: Using containers and Kubernetes helps bridge the gap between traditional mainframe services and modern deployment practices.

4. **Documentation Importance**: Clear documentation and examples make it easier for others to understand and learn from your work.

## Resources and References
- [IBM z/OS Connect EE Documentation](https://www.ibm.com/docs/en/zosconnect/3.0.x)
- [IBM Mainframe Modernization](https://www.ibm.com/cloud/modernization)
- [REST APIs and z/OS Connect](https://www.ibm.com/docs/en/zosconnect/zosconnect/3.0?topic=apis-creating-rest)
- [Docker Documentation](https://docs.docker.com/)
- [Kubernetes Documentation](https://kubernetes.io/docs/)

## Acknowledgments and Credits

This project is inspired by and references the following IBM resources:
- [IBM zosconnect GitHub Repository](https://github.com/zosconnect)
  - [sample-cics-api](https://github.com/zosconnect/sample-cics-api)
  - [sample-db2-api](https://github.com/zosconnect/sample-db2-api)
  - [sample-ims-api](https://github.com/zosconnect/sample-ims-api)

The mock services in this project are learning implementations based on these official IBM samples, created to understand mainframe modernization patterns and practices.