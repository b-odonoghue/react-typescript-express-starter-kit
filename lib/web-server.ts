import * as express from "express";
import * as http from "http";

const expressApp = express();
const portNumber = 5000;
const hostName = 'localhost';
const myUrl = `http://${hostName}:${portNumber}`;

expressApp.set('port', portNumber);
expressApp.use(express.static('lib/public'));
expressApp.use(express.static('lib/elasticsearch'));
expressApp.use(express.static('dist/public'));
expressApp.use(express.static('node_modules'));

var counter = 1;

http.createServer(expressApp).listen(
    portNumber,
    hostName,
    (error:any) => {
        if(error) {
            console.log(error);
            process.exit(1);
        }
        else console.log(`web server is running at ${myUrl}`);
    }
);