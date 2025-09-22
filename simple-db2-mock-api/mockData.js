// Mock data representing employee database
const employees = [
    {
        employeeNumber: "000010",
        firstName: "CHRISTINE",
        middleInitial: "I",
        lastName: "HAAS",
        department: "A00",
        phoneNumber: "3978",
        hireDate: "2000-01-01",
        job: "PRES",
        educationLevel: 18,
        sex: "F",
        dateOfBirth: "1933-08-14",
        salary: 52750.0,
        bonus: 1000.0,
        commission: 4220.0
    },
    {
        employeeNumber: "000020",
        firstName: "MICHAEL",
        middleInitial: "L",
        lastName: "THOMPSON",
        department: "B01",
        phoneNumber: "3476",
        hireDate: "2001-10-10",
        job: "MANAGER",
        educationLevel: 18,
        sex: "M",
        dateOfBirth: "1948-02-02",
        salary: 41250.0,
        bonus: 800.0,
        commission: 3300.0
    }
];

// Enum values for validation
const departments = ['A00', 'B01', 'C01', 'D11', 'D21', 'E01', 'E11', 'E21'];
const jobs = ['FIELDREP', 'OPERATOR', 'CLERK', 'DESIGNER', 'ANALYST', 'SALESREP', 'MANAGER', 'PRES'];

module.exports = {
    employees,
    departments,
    jobs
};
