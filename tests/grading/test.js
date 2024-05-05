import {Chess} from 'chess.js';
import * as fs from "fs";
import {spawn} from "child_process";

const __dirname = import.meta.dirname === undefined ? './tests/grading' : import.meta.dirname;
const path = `${__dirname}/pgns/`;

const swipl = (/^win/.test(process.platform) ? "C:\\Program Files\\swipl\\bin\\swipl" : "swipl");
const project = process.argv.length > 2 ? process.argv[2] : 'tests/dummy.pl';

console.log(`testing: ${project}`)

const chess = new Chess()

async function main() {
    let success = true;
    const files = fs.readdirSync(path);
    for (const file of files) {
        const result = await test(path + file);
        console.log(`${file}: ${result ? 'passed' : 'failed'}`);
        success = success && result;
    }
    return success;
}

function test(file) {
    return new Promise((resolve) => {
        const engine = spawn(swipl, ["-t", "halt", "-f", "-q", "-O", project, file]);
        engine.on('close', (code) => {
            if (code !== 0) {
                resolve(false);
            }
        });
        engine.stdout.on('data', (data) => {
            try {
                const pgn = data.toString().replace("\r", "").trim();
                chess.loadPgn(pgn);
                resolve(true);
            } catch (e) {}
        });
    });
}

const success = await main();
process.exit(success ? 0 : 1)
