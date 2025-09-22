const express = require('express');
const cors = require('cors');
const swaggerUi = require('swagger-ui-express');
const { employees, departments, jobs } = require('./mockData');

const app = express();
const port = 3001; // Different port from CICS mock

// Middleware
app.use(cors());
app.use(express.json());

// Helper function to format employee data
function formatEmployee(emp) {
    return {
        summary: {
            bio: `${emp.firstName} ${emp.lastName} was hired in ${new Date(emp.hireDate).getFullYear()} and is paid a total of $${(emp.salary + emp.bonus + emp.commission).toFixed(2)}`
        },
        personal: {
            firstName: emp.firstName,
            middleInitial: emp.middleInitial,
            lastName: emp.lastName,
            sex: emp.sex,
            dateOfBirth: emp.dateOfBirth
        },
        work: {
            employeeNumber: emp.employeeNumber,
            department: emp.department,
            phoneNumber: emp.phoneNumber,
            hireDate: emp.hireDate,
            job: emp.job,
            educationLevel: emp.educationLevel,
            pay: {
                salary: emp.salary,
                bonus: emp.bonus,
                commission: emp.commission
            }
        }
    };
}

// API Routes
// GET /employees - Get all employees with optional filters
app.get('/employees', (req, res) => {
    let result = [...employees];
    
    // Apply department filter
    if (req.query.department) {
        result = result.filter(emp => emp.department === req.query.department);
    }
    
    // Apply job filter
    if (req.query.job) {
        result = result.filter(emp => emp.job.trim() === req.query.job.trim());
    }
    
    res.json(result.map(formatEmployee));
});

// GET /employees/{id} - Get specific employee
app.get('/employees/:id', (req, res) => {
    const employee = employees.find(emp => emp.employeeNumber === req.params.id);
    
    if (!employee) {
        return res.status(404).json({
            message: "Employee could not be found"
        });
    }
    
    res.json(employee);
});

// POST /employees - Add new employee
app.post('/employees', (req, res) => {
    const newEmployee = req.body;
    
    // Basic validation
    if (!newEmployee.employeeNumber || !newEmployee.firstName || !newEmployee.lastName) {
        return res.status(400).json({
            message: "Missing required fields"
        });
    }
    
    // Validate department and job
    if (!departments.includes(newEmployee.department)) {
        return res.status(400).json({
            message: "Invalid department"
        });
    }
    
    if (!jobs.includes(newEmployee.job.trim())) {
        return res.status(400).json({
            message: "Invalid job"
        });
    }
    
    employees.push(newEmployee);
    res.status(201).json(newEmployee);
});

// PUT /employees/{id} - Update employee
app.put('/employees/:id', (req, res) => {
    const empIndex = employees.findIndex(emp => emp.employeeNumber === req.params.id);
    
    if (empIndex === -1) {
        return res.status(404).json({
            message: "Employee could not be found"
        });
    }
    
    // Update employee data
    employees[empIndex] = { ...employees[empIndex], ...req.body };
    res.json(employees[empIndex]);
});

// DELETE /employees/{id} - Delete employee
app.delete('/employees/:id', (req, res) => {
    const empIndex = employees.findIndex(emp => emp.employeeNumber === req.params.id);
    
    if (empIndex === -1) {
        return res.status(404).json({
            message: "Employee could not be found"
        });
    }
    
    const deletedEmployee = employees.splice(empIndex, 1)[0];
    res.json({ employeeNumber: deletedEmployee.employeeNumber });
});

// Basic API documentation endpoint
app.get('/', (req, res) => {
    res.json({
        message: "DB2 Employee Management API Mock",
        endpoints: {
            "GET /employees": "List all employees (optional filters: department, job)",
            "GET /employees/:id": "Get specific employee details",
            "POST /employees": "Add new employee",
            "PUT /employees/:id": "Update employee details",
            "DELETE /employees/:id": "Delete employee"
        }
    });
});

// Start server
app.listen(port, () => {
    console.log(`DB2 Mock API server running at http://localhost:${port}`);
});
