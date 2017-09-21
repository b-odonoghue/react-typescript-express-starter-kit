import * as React from "react";
import { Row, Column } from "./bootstrap";
import { getCounterValue, updateCounter } from "../../elasticsearch/elasticsearch";


//-----Home Page-----//
// Some static content
export const HomePage = () => (
    <div>
        <h1>Welcome!</h1>
        <p>Some text.</p>
    </div>
)

//-----Page 1-----//
// Some static content
interface PersonData {
    firstName:string,
    lastName:string,
    favoriteColors:string[]
}

export interface PersonProps {
    person:PersonData
}

export const Person = (props:PersonProps) => <div>
    <h4>{props.person.firstName} {props.person.lastName}</h4>
    <ul>
        {props.person.favoriteColors.map(color => <li key={color}>{color}</li>)}
    </ul>
</div>;

const mikesFavoritePeople:PersonData[] = [
    {
        firstName: "Tommy",
        lastName: "Sullivan",
        favoriteColors: ["red",'blue']
    },
    {
        firstName: "Bobby",
        lastName: "Chen",
        favoriteColors: ["green",'orange']
    }
]

const words = ["hi","bye",'hello','goodbye'];

export interface IPageProps {
    heading:string;
}

export const Page1 = (props:IPageProps) => (
    <div>
        <h1>Mike Chen Resume {props.heading}</h1>
        <Row>
            {words.map(word => <Column columnWidthForMediumScreen={5} ><p className="fa ">{word}</p></Column>)}
        </Row>
        <ul>
            {mikesFavoritePeople.map(person => <li key={person.firstName}><Person person={person} /></li>)}
        </ul>
    </div>
)

//-----Page 2-----//
// Button that increases by 1 every time you click it. Elasticsearch database hosted at localhost:9200
const backgroundStyle = {
    padding: 50,
    backgroundColor: "#00F000",
    width: 250,
    height: 150,
    borderRadius: 10,
    textAlign: "center"
};

const buttonStyle = {
    fontSize: "1em",
    width: 30,
    height: 30,
    fontFamily: "cambria",
    color: "#333",
    lineHeight: "3px"
};

class Counter extends React.Component<{display?:number}, {}> {
  render() {
    const textStyle = {
        fontSize: 14,
        fontFamily: "cambria",
        color: "#333"
    };
 
    return <div style={textStyle}>
        {this.props.display}
    </div>
  }
};

export class Page2 extends React.Component<{}, {counter?:number}> {
    constructor() {
        super();
        this.state = {}
    }

    async getCounterValueAndSetState() {
        getCounterValue().then(x => this.setState({counter:x}));
    }

    componentWillMount() {
        this.getCounterValueAndSetState();
    }

    async onClick() {
        const counterValue = await getCounterValue();
        await updateCounter(counterValue, 1);
        this.getCounterValueAndSetState();
    }

    render() { 
        return <div style={backgroundStyle}>
            <Counter display={this.state.counter} />
            <button style={buttonStyle} onClick={() => this.onClick()}>+</button>
        </div>
    }
}

//-----Page 3-----//
//Bouncing square using DOM
const boxHeight = 400;
const boxWidth = 500;
const bouncyBoxWidth = 40;
const bouncyBoxHeight = 40;

export class Box extends React.Component {
    render () {      
        const divStyle = {
            display: "block",
            margin: "20px auto",
            border: "1px solid #666",
            height: `${boxHeight}px`,
            width: `${boxWidth}px`
        };
        return <div style={divStyle}>
            <BouncyBox/>
        </div>
    }
}

export class BouncyBox extends React.Component <{}, {x:number, y:number, vx:number, vy:number}> {
    constructor() {
        super();
        this.state = {x: 50, y: 50, vx: 2, vy: 2};
    }
    
    moveBox() {
        this.setState({x: this.state.x + this.state.vx, y: this.state.y + this.state.vy});
        if (this.state.x >= boxWidth - bouncyBoxWidth || this.state.x <= 0) this.setState({vx: this.state.vx * -1})
        if (this.state.y >= boxHeight - bouncyBoxHeight || this.state.y <= 0) this.setState({vy: this.state.vy * -1})
    }

    componentDidMount() {
        looptimer = setInterval(this.moveBox.bind(this), 25);
    }

    componentWillUnmount() {
        clearInterval(looptimer);
    }

    render() {
        const boxStyle = {
            width: `${bouncyBoxWidth}px`,
            height: `${bouncyBoxHeight}px`,
            backgroundColor: "blue",
            position: ("relative" as "relative"), //TODO: wtf? typescript compiler issue?
            top: `${this.state.y}px`,
            left: `${this.state.x}px`,
        };
        return <div style={boxStyle}/>
    };
}

export const Page3 = () => <Box/>;

//-----Page 4-----//
// Bouncing ball using canvas. Stole this from the internet and added some changes
// Source: https://www.burakkanber.com/blog/modeling-physics-javascript-gravity-and-drag/
var mouse = {x: 0, y: 0, isDown: false};
var looptimer;
var ctx;
const canvasHeight = 400;
const canvasWidth = 500;
var ball = {
    position: {x: canvasWidth/2, y: 0},
    velocity: {x: 10, y: 0},
    mass: 0.1, //kg
    radius: 15, // 1px = 1cm
    restitution: -1
};
const Cd = 0.47;  // Dimensionless
const rho = .1;//.5; // kg / m^3
const A = Math.PI * ball.radius * ball.radius / (10000); // m^2
const ag = 9.81;  // m / s^2
const frameRate = 1/40; // Seconds
const frameDelay = frameRate * 1000; // ms

function drawCircle(ctx:any, x:number, y:number, radius:number) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI*2, true);
    ctx.fill();
    ctx.closePath();
}

export class Canvas extends React.Component<{}, {}> {
    componentDidMount() {
        ctx = (this.refs.canvas as HTMLCanvasElement).getContext('2d');
        this.updateCanvas();
        looptimer = setInterval(this.moveBall.bind(this), frameDelay);
    }

    componentWillUnmount() {
        clearInterval(looptimer);
    }

    updateCanvas() {
        ctx.clearRect(0,0, canvasWidth, canvasHeight);
        ctx.save();
        drawCircle(ctx, ball.position.x, ball.position.y, ball.radius);
    }

    getMousePosition(e) {
        mouse.x = e.pageX - canvas.offsetLeft;
        mouse.y = e.pageY - canvas.offsetTop;
    }

    mouseDown(e) {
        this.getMousePosition(e);
        mouse.isDown = true;
        ball.position.x = mouse.x;
        ball.position.y = mouse.y;
    }

    mouseUp(e) {
        mouse.isDown = false;
        ball.velocity.y = (ball.position.y - mouse.y) / 10;
        ball.velocity.x = (ball.position.x - mouse.x) / 10;
    }

    mouseScroll(e:any) {
        const growthValue = 3;
        if (e.deltaY < 0 && ball.radius <= canvasWidth / 2 - growthValue) {
            ball.radius += growthValue;
        }
        else if (e.deltaY > 0 && ball.radius > growthValue) {
            ball.radius -= growthValue;
        }
    }

    moveBall() {
        if (!mouse.isDown) {
            // Do physics
            // Drag force: Fd = -1/2 * Cd * A * rho * v * v
            var Fx = -0.5 * Cd * A * rho * ball.velocity.x * ball.velocity.x * ball.velocity.x / Math.abs(ball.velocity.x);
            var Fy = -0.5 * Cd * A * rho * ball.velocity.y * ball.velocity.y * ball.velocity.y / Math.abs(ball.velocity.y);
            
            Fx = (isNaN(Fx) ? 0 : Fx);
            Fy = (isNaN(Fy) ? 0 : Fy);
            
            // Calculate acceleration ( F = ma )
            var ax = Fx / ball.mass;
            var ay = ag + (Fy / ball.mass);
                // Integrate to get velocity
            ball.velocity.x += ax*frameRate;
            ball.velocity.y += ay*frameRate;
            
            // Integrate to get position
            ball.position.x += ball.velocity.x*frameRate*100;
            ball.position.y += ball.velocity.y*frameRate*100;
        }
        // Handle collisions
        if (ball.position.y > canvasHeight - ball.radius) {
            ball.velocity.y *= ball.restitution;
            ball.position.y = canvasHeight - ball.radius;
        }
        if (ball.position.x > canvasWidth - ball.radius) {
            ball.velocity.x *= ball.restitution;
            ball.position.x = canvasWidth - ball.radius;
        }
        if (ball.position.x < ball.radius) {
            ball.velocity.x *= ball.restitution;
            ball.position.x = ball.radius;
        }
        this.updateCanvas()
        if (mouse.isDown) {
            ctx.beginPath();
            ctx.moveTo(ball.position.x, ball.position.y);
            ctx.lineTo(mouse.x, mouse.y);
            ctx.stroke();
            ctx.closePath();
        }
    }
    
    render () {
        const canvasStyle = {
            //display: "block",
            margin: "20px auto",
            border: "1px solid #666"
        };
        return <canvas
            id="canvas"
            ref="canvas"
            height={canvasHeight}
            width={canvasWidth}
            style={canvasStyle}
            onMouseMove={e => this.getMousePosition(e)}
            onMouseDown={this.mouseDown.bind(this)}
            onMouseUp={this.mouseUp.bind(this)}
            onWheel={this.mouseScroll.bind(this)}>
        </canvas>
    }
}

export const Page4 = () => <Canvas>Browser does not support canvas.</Canvas>

export const Page5 = () => <div/>