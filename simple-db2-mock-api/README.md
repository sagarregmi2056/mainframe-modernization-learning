# DB2 Employee Service Mock

## Overview
This service simulates a DB2-based employee management system, demonstrating how DB2 database operations can be exposed as REST APIs.

## DB2 Concepts Demonstrated
- SQL operations
- Relational data structures
- Query parameters
- Data filtering

## API Endpoints

### Get All Employees
```
GET /employees
Query Parameters:
- department: Filter by department
- job: Filter by job title

Response:
[{
    "summary": {
        "bio": string
    },
    "personal": EmployeePersonal,
    "work": EmployeeWork
}]
```

### Get Employee by ID
```
GET /employees/{id}
Parameters:
- id: Employee number

Response:
{
    "employeeNumber": string,
    "firstName": string,
    "lastName": string,
    ...
}
```

### Add Employee
```
POST /employees
Body: {
    "employeeNumber": string,
    "firstName": string,
    "lastName": string,
    ...
}
```

### Update Employee
```
PUT /employees/{id}
Parameters:
- id: Employee number
Body: Updated employee data
```

### Delete Employee
```
DELETE /employees/{id}
Parameters:
- id: Employee number
```

## Data Structures

### DB2 to JSON Mapping
```
DB2 Table Structure:
EMPLOYEE
- EMPNO CHAR(6)
- FIRSTNME VARCHAR(12)
- LASTNAME VARCHAR(15)
- WORKDEPT CHAR(3)
...

↓ Transforms to ↓

JSON Structure:
{
    "employeeNumber": string,
    "firstName": string,
    "lastName": string,
    "department": string,
    ...
}
```

## Installation

1. Install dependencies:
```bash
npm install
```

2. Start the server:
```bash
npm start
```

## Configuration

### Environment Variables
```
PORT=3001 (default)
```

## Usage Examples

### Get All Employees
```bash
curl http://localhost:3001/employees
```

### Get Employees by Department
```bash
curl http://localhost:3001/employees?department=A00
```

### Add New Employee
```bash
curl -X POST http://localhost:3001/employees \
  -H "Content-Type: application/json" \
  -d '{
    "employeeNumber": "000030",
    "firstName": "JOHN",
    "lastName": "DOE",
    "department": "A00"
  }'
```

## Development

### Project Structure
```
simple-db2-mock-api/
├── server.js       # Main application file
├── mockData.js     # Simulated employee data
├── package.json    # Dependencies and scripts
└── Dockerfile     # Container configuration
```

### Mock Data Format
```javascript
{
    "employeeNumber": "000010",
    "firstName": "CHRISTINE",
    "lastName": "HAAS",
    "department": "A00",
    "job": "PRES",
    ...
}
```

## Testing
```bash
# Test service health
curl http://localhost:3001/

# Get all employees
curl http://localhost:3001/employees
```

## Docker Support

### Build
```bash
docker build -t db2-mock-api .
```

### Run
```bash
docker run -p 3001:3001 db2-mock-api
```

## Error Handling
- Invalid employee numbers
- Duplicate entries
- Invalid department codes
- System errors

## Integration
This service is designed to be used with the API Gateway but can also be used standalone.

## DB2 Learning Resources
- [IBM Db2 Documentation](https://www.ibm.com/docs/en/db2)
- [SQL Reference](https://www.ibm.com/docs/en/db2-for-zos/12?topic=sql-reference)
- [DB2 for z/OS](https://www.ibm.com/docs/en/db2-for-zos)
