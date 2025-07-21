// Quick fix for COBOL file I/O
const fs = require('fs').promises;
const { exec } = require('child_process');
const { promisify } = require('util');
const execAsync = promisify(exec);

async function processCalculation(calcData, requestId) {
    const inputFile = `calc-input-${requestId}.dat`;
    const outputFile = `calc-output-${requestId}.dat`;
    
    try {
        // Prepare input data
        const inputData = prepareCobolInput(calcData);
        
        // Write to the temporary file
        await fs.writeFile(inputFile, inputData);
        
        // Copy to expected COBOL filenames
        await fs.copyFile(inputFile, 'calc-input.dat');
        
        // Execute COBOL program
        await execAsync('./financial-calc');
        
        // Copy output to temporary file
        await fs.copyFile('calc-output.dat', outputFile);
        
        // Read and parse output
        const output = await fs.readFile(outputFile, 'utf8');
        const result = parseCobolOutput(output);
        
        // Cleanup
        await Promise.all([
            fs.unlink(inputFile).catch(() => {}),
            fs.unlink(outputFile).catch(() => {}),
            fs.unlink('calc-input.dat').catch(() => {}),
            fs.unlink('calc-output.dat').catch(() => {})
        ]);
        
        return result;
    } catch (error) {
        // Cleanup on error
        await Promise.all([
            fs.unlink(inputFile).catch(() => {}),
            fs.unlink(outputFile).catch(() => {}),
            fs.unlink('calc-input.dat').catch(() => {}),
            fs.unlink('calc-output.dat').catch(() => {})
        ]);
        throw error;
    }
}

// Export the fixed function
module.exports = { processCalculation };