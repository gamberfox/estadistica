"use strict";
class Robot {
    x;
    y;
    previousX;
    previousY;
    gridWidth;
    gridHeight;
    difficulty;
    previousCell;
    cell;
    passengerWasFound;
    constructor(x, y, gridWidth, gridHeight, difficulty) {
        this.previousX = x;
        this.previousY = y;
        this.x = x;
        this.y = y;
        this.gridWidth = gridWidth;
        this.gridHeight = gridHeight;
        this.difficulty = difficulty;
        this.previousCell = 'cell0';
        this.cell = map?.querySelector(`[data-x="${this.previousX}"][data-y="${this.previousY}"]`);
        this.passengerWasFound = false;
    }
    updateGrid() {
        if (this.previousCell === 'cell5') {
            this.cell.className = 'cell0';
        }
        else {
            this.cell.className = this.previousCell;
        }
        this.cell = map?.querySelector(`[data-x="${this.x}"][data-y="${this.y}"]`);
        this.previousCell = this.cell.className;
        //if(this.cell.className==='cell6') alert('you won, congrats');
        if (this.previousCell === 'cell5')
            this.passengerWasFound = true;
        if (this.passengerWasFound) {
            this.cell.className = 'cell7';
        }
        else {
            this.cell.className = 'cell2';
        }
        this.previousX = this.x;
        this.previousY = this.y;
    }
    moveUp() {
        if (this.y > 0) {
            this.y -= 1;
            this.updateGrid();
        }
        else {
            alert("Can't move up, at top boundary.");
        }
    }
    moveDown() {
        if (this.y < this.gridHeight - 1) {
            this.y += 1;
            this.updateGrid();
        }
        else {
            alert("Can't move down, at bottom boundary.");
        }
    }
    moveLeft() {
        if (this.x > 0) {
            this.x -= 1;
            this.updateGrid();
        }
        else {
            alert("Can't move left, at left boundary.");
        }
    }
    moveRight() {
        if (this.x < this.gridWidth - 1) {
            //this.taxiCell.className='cell0';
            this.x += 1;
            this.updateGrid();
        }
        else {
            alert("Can't move right, at right boundary.");
        }
    }
    delay(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }
    async followDirections(listOfMovements) {
        console.log(`Im gonna start following directions`);
        for (let direction of listOfMovements) { //right=0, up=1, left=2, down=3.
            switch (direction) {
                case 0:
                    //setTimeout(()=> this.moveRight(),1000);
                    this.moveRight();
                    break;
                case 1:
                    this.moveUp();
                    break;
                case 2:
                    this.moveLeft();
                    break;
                case 3:
                    this.moveDown();
                    break;
            }
            await this.delay(this.difficulty);
        }
    }
}
