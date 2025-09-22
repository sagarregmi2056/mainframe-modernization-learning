# DB2 Data Flow and Mapping Guide

## 1. Complete Request Flow

```
Client Request → API Gateway → DB2 Service → DB2 Database
     ↓              ↓             ↓             ↓
JSON/REST    Route to DB2    SQL Query     COBOL Program
     ↓              ↓             ↓             ↓
  Request     Transform      Execute      Fetch Results
     ↓              ↓             ↓             ↓
  Response  ←   JSON     ←   Results   ←  COBOL Data
```

## 2. Detailed Flow Example

### Step 1: Client REST Request
```http
GET /employees?department=A00&job=MANAGER
```

### Step 2: API Gateway Processing
```javascript
// In API Gateway
const db2Proxy = createProxyMiddleware('/employees', {
    target: 'http://localhost:3001',
    onProxyReq: (proxyReq, req, res) => {
        // Transform REST query to SQL-like format
        console.log('Transforming REST request to SQL query format');
    }
});
```

### Step 3: DB2 Service Processing
```sql
-- Generated SQL Query
SELECT * FROM DSN8B10.EMP
WHERE WORKDEPT = 'A00'
  AND JOB = 'MANAGER'
```

### Step 4: COBOL Program Interface
```cobol
       WORKING-STORAGE SECTION.
       01  DCLEMPLOYEE.
           03 EMPNO    PIC X(6).
           03 FIRSTNME PIC X(12).
           03 LASTNAME PIC X(15).
           03 WORKDEPT PIC X(3).
           03 PHONENO  PIC X(4).
           03 HIREDATE PIC X(10).
           03 JOB      PIC X(8).
           03 EDLEVEL  PIC S9(4) COMP.
           03 SEX      PIC X(1).
           03 SALARY   PIC S9(7)V9(2) COMP-3.
           03 BONUS    PIC S9(7)V9(2) COMP-3.
```

### Step 5: Data Transformation
```javascript
// COBOL to JSON Transformation
const transformEmployeeData = (cobolData) => {
    return {
        employeeNumber: cobolData.EMPNO.trim(),
        firstName: cobolData.FIRSTNME.trim(),
        lastName: cobolData.LASTNAME.trim(),
        department: cobolData.WORKDEPT.trim(),
        phoneNumber: cobolData.PHONENO.trim(),
        hireDate: formatDate(cobolData.HIREDATE),
        job: cobolData.JOB.trim(),
        educationLevel: parseInt(cobolData.EDLEVEL),
        sex: cobolData.SEX,
        salary: parseComp3(cobolData.SALARY),
        bonus: parseComp3(cobolData.BONUS)
    };
};
```

## 3. Data Type Mappings

### COBOL to JSON Type Mappings
```
COBOL                   →  JSON/JavaScript
------------------------------------------
PIC X(n)               →  String (trimmed)
PIC 9(n)               →  Number
PIC S9(n)V9(m) COMP-3  →  Number (decimal)
PIC S9(n) COMP         →  Number (integer)
GROUP items            →  Object
OCCURS n TIMES         →  Array
```

### Example Transformations

1. **String Fields**:
```cobol
FIRSTNME PIC X(12) VALUE 'JOHN        '
```
```javascript
firstName: "JOHN" // Trimmed
```

2. **Numeric Fields**:
```cobol
SALARY PIC S9(7)V9(2) COMP-3 VALUE +0050000.00
```
```javascript
salary: 50000.00
```

3. **Group Items**:
```cobol
01 EMPLOYEE-RECORD.
   03 PERSONAL-INFO.
      05 FIRSTNME PIC X(12).
      05 LASTNAME PIC X(15).
   03 WORK-INFO.
      05 DEPT    PIC X(3).
      05 JOB     PIC X(8).
```
```javascript
{
    personalInfo: {
        firstName: "...",
        lastName: "..."
    },
    workInfo: {
        department: "...",
        job: "..."
    }
}
```

## 4. Real-World Flow (with z/OS Connect)

```
1. Client Request
   GET /employees?department=A00

2. z/OS Connect
   - Authenticates request
   - Transforms to DB2 format
   - Manages connection pool

3. DB2 for z/OS
   - Executes SQL
   - Returns result set

4. COBOL Program
   - Processes result set
   - Formats data

5. z/OS Connect
   - Transforms COBOL to JSON
   - Sends response
```

## 5. Common Data Scenarios

### Scenario 1: Get Employee
```http
GET /employees/000010
```
```sql
SELECT * FROM DSN8B10.EMP WHERE EMPNO = '000010'
```
```cobol
EXEC SQL
    SELECT EMPNO, FIRSTNME, LASTNAME, WORKDEPT
    INTO :DCLEMPLOYEE.EMPNO,
         :DCLEMPLOYEE.FIRSTNME,
         :DCLEMPLOYEE.LASTNAME,
         :DCLEMPLOYEE.WORKDEPT
    FROM DSN8B10.EMP
    WHERE EMPNO = :HOST-EMPNO
END-EXEC
```
```javascript
// Final JSON Response
{
    "employeeNumber": "000010",
    "firstName": "CHRISTINE",
    "lastName": "HAAS",
    "department": "A00",
    // ... other fields
}
```

### Scenario 2: Update Employee
```http
PUT /employees/000010
Content-Type: application/json
{
    "job": "MANAGER",
    "salary": 52000.00
}
```
```sql
UPDATE DSN8B10.EMP 
SET JOB = 'MANAGER', 
    SALARY = 52000.00
WHERE EMPNO = '000010'
```

## 6. Error Handling and Validation

### SQL Error Mapping
```javascript
// DB2 SQL Code to REST Response
const sqlErrorMapping = {
    '-204': { status: 404, message: 'Resource not found' },
    '-803': { status: 409, message: 'Duplicate key value' },
    '-530': { status: 403, message: 'Authorization failed' }
};
```

### Data Validation
```javascript
// Before SQL execution
const validateEmployee = (data) => {
    if (data.employeeNumber && !/^\d{6}$/.test(data.employeeNumber)) {
        throw new Error('Invalid employee number format');
    }
    if (data.department && !validDepartments.includes(data.department)) {
        throw new Error('Invalid department code');
    }
    // ... more validations
};
```

Would you like me to:
1. Add more detailed examples of specific transformations?
2. Show more error handling scenarios?
3. Explain the connection pooling and transaction management?
4. Add more COBOL to JSON mapping examples?

Let me know what aspect you'd like to explore further!
