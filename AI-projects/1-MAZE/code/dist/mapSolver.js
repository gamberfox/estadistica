"use strict";
let answer = [-33];
const RIGHT = 0;
const UP = 1;
const LEFT = 2;
const DOWN = 3;
const travelCost = { [FREE_CELL.toString()]: 1,
    [(VEHICLE).toString()]: 1,
    [(MEDIUM_TRAFFIC).toString()]: 4,
    [(HEAVY_TRAFFIC).toString()]: 7,
    [(PASSENGER).toString()]: 1,
    [(DESTINATION).toString()]: 1
};
const STOP_POINT = 100_000;
let statSheet;
// Handle button click to perform the selected action
solveButton.addEventListener('click', () => {
    const startTime = performance.now();
    const selectedAction = actionSelect.value; // Get the selected action
    const file = fileInput.files?.[0]; // Get the selected file
    console.log(`here`);
    nutcrackerAudio.currentTime = 2;
    if (!nutcrackerAudio.paused) {
        nutcrackerAudio.pause();
        nutcrackerAudio.currentTime = 2;
    }
    if (!joySongAudio.paused) {
        joySongAudio.pause();
        joySongAudio.currentTime = 1;
    }
    //nutcrackerAudio.play();
    if (file) {
        const reader = new FileReader(); // Create a new FileReader
        statSheet = new StatSheet();
        switch (selectedAction) {
            case 'avara':
                // Read and display the file content
                /* reader.onload = (e) => {
                    fileContent.textContent = e.target?.result as string; // Display the file content
                }; */
                //reader.readAsText(file); // changes the bottom text
                break;
            case 'aStar':
                // Display the file size in bytes
                fileContent.textContent = `File Size: ${file.size} bytes`;
                break;
            case 'breadth':
                answer = breadthSolver();
                console.log(`breadth case`);
                break;
            case 'uniformCost':
                reader.readAsDataURL(file); // Read the file as a data URL (for image preview)
                answer = uniformCostSolver();
                console.log(`the uniform cost algoritm was used`);
                break;
            case 'depthAvoidingCycles':
                break;
            default:
                fileContent.textContent = 'Please select a valid action.';
                break;
        }
        console.log('solveButton listener');
        console.log(`${answer}`);
        let newAnswer = [];
        for (let direction of answer) {
            switch (direction) {
                case RIGHT:
                    newAnswer.push(LEFT);
                    break;
                case UP:
                    newAnswer.push(DOWN);
                    break;
                case LEFT:
                    newAnswer.push(RIGHT);
                    break;
                case DOWN:
                    newAnswer.push(UP);
                    break;
            }
        }
        answer = newAnswer;
        statSheet.computeTime = performance.now() - startTime;
        console.log('we have a new answer');
        console.log(answer);
        console.log(`expanded nodes: ${statSheet.expandedNodes}`);
        console.log(`node depth: ${statSheet.nodeDepth}`);
        console.log(`time cost: ${statSheet.solutionCost} milliseconds`);
        console.log(`solution cost: ${statSheet.solutionCost}`);
        reader.onerror = () => {
            fileContent.textContent = 'Error reading file!';
        };
    }
    else {
        fileContent.textContent = 'No file selected.';
    }
});
class MazePosition {
    x;
    y;
    cellType;
    nodeDepth;
    //pos0=y,pos1=x,pos2=x+y.
    //radarDistance:number;
    lDistance = 0;
    travelCost = 0;
    visited = false;
    foundPassenger;
    foundPassengerThisRound = false;
    father = null;
    pathToFather = null;
    pathHome = [];
    //the taxi can go to a maximum of 4 directions
    rightChild = null;
    upChild = null;
    leftChild = null;
    downChild = null;
    constructor(posX, posY, cellType, nodeDepth, foundPassenger) {
        this.x = posX;
        this.y = posY;
        this.cellType = cellType;
        this.nodeDepth = nodeDepth;
        this.foundPassenger = foundPassenger;
        //this.radarDistance=[posX,posY,0];
    }
}
class StatSheet {
    expandedNodes = 0;
    nodeDepth = 0;
    computeTime = 0;
    solutionCost = 0;
}
