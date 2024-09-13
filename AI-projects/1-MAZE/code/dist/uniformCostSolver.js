"use strict";
//solveByBreadth.addEventListener('click', () => {
//
//mapMesh   from mapReader.ts
//let interation=0;
const uniformCostSolver = () => {
    console.log(`start uniform cost solver`);
    let foundAnswer = false;
    let currentNode = new MazePosition(vehiclePosition[0], vehiclePosition[1], VEHICLE, 0, false);
    let queue = [currentNode];
    while (queue.length > 0) {
        if (statSheet.expandedNodes > STOP_POINT) {
            console.log(`${statSheet.expandedNodes} nodes were searched,
                the limit has been reached`);
            break;
        }
        statSheet.expandedNodes += 1;
        queue.sort((a, b) => a.travelCost - b.travelCost);
        currentNode = queue.shift();
        statSheet.nodeDepth = (statSheet.nodeDepth < currentNode.nodeDepth) ?
            currentNode.nodeDepth : statSheet.nodeDepth;
        //we'll check if we found the answer
        if (currentNode.foundPassenger &&
            DESTINATION === mapMesh[currentNode.y][currentNode.x]) {
            foundAnswer = true;
            statSheet.solutionCost = currentNode.travelCost;
            console.log(`an answer was found`);
            break;
        }
        //we'll check every neighbor
        if (WALL !== mapMesh[currentNode.y][currentNode.x + 1] //no going to walls
            && (null === currentNode.pathToFather
                || RIGHT !== currentNode.pathToFather
                || currentNode.foundPassengerThisRound)) {
            currentNode.rightChild = new MazePosition(currentNode.x + 1, currentNode.y, mapMesh[currentNode.y][currentNode.x + 1], currentNode.nodeDepth + 1, currentNode.foundPassenger);
            queue.push(currentNode.rightChild);
            if (PASSENGER === mapMesh[currentNode.y][currentNode.x + 1]) {
                currentNode.rightChild.foundPassenger = true;
                currentNode.rightChild.foundPassengerThisRound = true;
            }
            currentNode.rightChild.father = currentNode;
            currentNode.rightChild.pathToFather = LEFT;
            /* statSheet.nodeDepth=
            (currentNode.rightChild.nodeDepth>statSheet.nodeDepth)?
            currentNode.rightChild.nodeDepth : statSheet.nodeDepth; */
            currentNode.rightChild.travelCost =
                currentNode.travelCost
                    + travelCost[(mapMesh[currentNode.y][currentNode.x + 1]).toString()];
        }
        //trying to go UP
        if (WALL !== mapMesh[currentNode.y - 1][currentNode.x] //no going to walls
            && (null === currentNode.pathToFather
                || UP !== currentNode.pathToFather
                || currentNode.foundPassengerThisRound)) {
            currentNode.upChild = new MazePosition(currentNode.x, currentNode.y - 1, mapMesh[currentNode.y - 1][currentNode.x], currentNode.nodeDepth + 1, currentNode.foundPassenger);
            queue.push(currentNode.upChild);
            if (PASSENGER === mapMesh[currentNode.y - 1][currentNode.x]) {
                currentNode.upChild.foundPassenger = true;
                currentNode.upChild.foundPassengerThisRound = true;
            }
            currentNode.upChild.father = currentNode;
            currentNode.upChild.pathToFather = DOWN;
            /* statSheet.nodeDepth=
            (currentNode.upChild.nodeDepth>statSheet.nodeDepth)?
            currentNode.upChild.nodeDepth : statSheet.nodeDepth; */
            currentNode.upChild.travelCost =
                currentNode.travelCost
                    + travelCost[(mapMesh[currentNode.y - 1][currentNode.x]).toString()];
        }
        //trying to go LEFT
        if (WALL !== mapMesh[currentNode.y][currentNode.x - 1] //no going to walls
            && (null === currentNode.pathToFather
                || LEFT !== currentNode.pathToFather
                || currentNode.foundPassengerThisRound)) {
            currentNode.leftChild = new MazePosition(currentNode.x - 1, currentNode.y, mapMesh[currentNode.y][currentNode.x - 1], currentNode.nodeDepth + 1, currentNode.foundPassenger);
            queue.push(currentNode.leftChild);
            if (PASSENGER === mapMesh[currentNode.y][currentNode.x - 1]) {
                currentNode.leftChild.foundPassenger = true;
                currentNode.leftChild.foundPassengerThisRound = true;
            }
            currentNode.leftChild.father = currentNode;
            currentNode.leftChild.pathToFather = RIGHT;
            /* statSheet.nodeDepth=
            (currentNode.leftChild.nodeDepth>statSheet.nodeDepth)?
            currentNode.leftChild.nodeDepth : statSheet.nodeDepth;
 */
            currentNode.leftChild.travelCost =
                currentNode.travelCost
                    + travelCost[(mapMesh[currentNode.y][currentNode.x - 1]).toString()];
        }
        //trying to go DOWN
        if (WALL !== mapMesh[currentNode.y + 1][currentNode.x] //no going to walls
            && (null === currentNode.pathToFather
                || DOWN !== currentNode.pathToFather
                || currentNode.foundPassengerThisRound)) {
            currentNode.downChild = new MazePosition(currentNode.x, currentNode.y + 1, mapMesh[currentNode.y + 1][currentNode.x], currentNode.nodeDepth + 1, currentNode.foundPassenger);
            queue.push(currentNode.downChild);
            if (PASSENGER === mapMesh[currentNode.y + 1][currentNode.x]) {
                currentNode.downChild.foundPassenger = true;
                currentNode.downChild.foundPassengerThisRound = true;
            }
            currentNode.downChild.father = currentNode;
            currentNode.downChild.pathToFather = UP;
            /* statSheet.nodeDepth=
            (currentNode.downChild.nodeDepth>statSheet.nodeDepth)?
            currentNode.downChild.nodeDepth : statSheet.nodeDepth;
 */
            currentNode.downChild.travelCost =
                currentNode.travelCost
                    + travelCost[(mapMesh[currentNode.y + 1][currentNode.x]).toString()];
        }
    }
    //build answer if you found it
    if (foundAnswer) {
        let path = [];
        let node = currentNode;
        console.log('answering');
        while (node !== null && null !== node.pathToFather) {
            path.push(node.pathToFather);
            node = node.father;
        }
        path.reverse();
        return path;
    }
    return [-1];
};
