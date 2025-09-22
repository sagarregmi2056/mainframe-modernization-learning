// Simple monitoring middleware to demonstrate API metrics
const metrics = {
    requests: {
        total: 0,
        byService: {
            cics: 0,
            db2: 0,
            ims: 0
        },
        byMethod: {
            GET: 0,
            POST: 0,
            PUT: 0,
            DELETE: 0
        }
    },
    errors: {
        total: 0,
        byService: {
            cics: 0,
            db2: 0,
            ims: 0
        }
    },
    responseTime: {
        total: 0,
        count: 0,
        average: 0
    }
};

function updateMetrics(service, method, duration, isError) {
    metrics.requests.total++;
    metrics.requests.byService[service]++;
    metrics.requests.byMethod[method]++;
    
    if (isError) {
        metrics.errors.total++;
        metrics.errors.byService[service]++;
    }

    metrics.responseTime.total += duration;
    metrics.responseTime.count++;
    metrics.responseTime.average = metrics.responseTime.total / metrics.responseTime.count;
}

function monitoringMiddleware(service) {
    return (req, res, next) => {
        const start = Date.now();
        
        // Capture response
        const originalEnd = res.end;
        res.end = function() {
            const duration = Date.now() - start;
            const isError = res.statusCode >= 400;
            
            updateMetrics(service, req.method, duration, isError);
            
            // Log request
            console.log(`[${new Date().toISOString()}] ${service.toUpperCase()} - ${req.method} ${req.url} - ${res.statusCode} - ${duration}ms`);
            
            originalEnd.apply(res, arguments);
        };
        
        next();
    };
}

function getMetrics() {
    return {
        ...metrics,
        responseTime: {
            ...metrics.responseTime,
            average: Math.round(metrics.responseTime.average)
        }
    };
}

module.exports = {
    monitoringMiddleware,
    getMetrics
};
