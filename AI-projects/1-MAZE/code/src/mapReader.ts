
const FREE_CELL=0;
const WALL=1;
const VEHICLE=2;
const MEDIUM_TRAFFIC=3;
const HEAVY_TRAFFIC=4;
const PASSENGER=5;
const DESTINATION=6;

const EASY:number=1000;
const NORMAL:number=500;
const HARD:number=100;

let vehiclePosition:number[];
let passengerPosition:number[];
let destinationPosition:number[];
let mapMesh:number[][];
let treeMesh:MazePosition[][];//yet to be implemented
//this returns a mesh
function parseMap(text:string){
    if(mapInfo!==null){
        //mapInfo.textContent=text; //makes that big square
        mapInfo.textContent="I'll start parsing the map";
    }
    let rows:string[]=text.split(/\r?\n|\r/);
    let mapRow:number[];
    mapRow=(Array(rows[0].split(' ').length+2).fill(WALL));
    mapMesh=[mapRow];
    for(let line of rows){
        // console.log(`${line}`);
        
        let row=[WALL];
        for(let i of line.split(' ')){
            row.push(parseInt(i));
        }
        row.push(WALL);
        if(row.length>3){
            mapMesh.push(row);
        }
    }
    mapRow=(Array(rows[0].split(' ').length+2).fill(WALL));
    mapMesh.push(mapRow);
    return mapMesh;
}

const drawMap=()=>{
    let cell:HTMLElement;
    for(let x=1;x<11;x++){
        for(let y=1;y<11;y++){
            console.log(`passy`);
            cell=map?.querySelector(`[data-x="${x-1}"][data-y="${y-1}"]`) as HTMLElement;
            if(mapMesh===undefined){
                console.log(`hello from undefined`);
            }
            // console.log(`${mapMesh.length}`);
            // console.log(`${mapMesh[0].length}`);
            switch(mapMesh[y][x]){
                case 0:
                    //cell.classList.add('cell0');
                    cell.className='cell0';
                    break;
                case 1:
                    //cell.classList.add('cell1');
                    cell.className='cell1';
                    break;
                case 2:
                    //cell.classList.add('cell2');
                    cell.className='cell2';
                    vehiclePosition=[x,y];
                    robot =new Robot(x-1,y-1,10,10,NORMAL);
                    break;
                case 3:
                    //cell.classList.add('cell3');
                    cell.className='cell3';
                    break;
                case 4:
                    //cell.classList.add('cell4');
                    cell.className='cell4';
                    break;
                case 5:
                    //cell.classList.add('cell5');
                    cell.className='cell5';
                    passengerPosition=[x,y];
                    break;
                case 6:
                    //cell.classList.add('cell6');
                    cell.className='cell6';
                    destinationPosition=[x,y];
                    break;
                case 7:
                    //cell.classList.add('cell7');
                    cell.className='cell7';
                    break;
                default:
                    //cell.classList.add('cell8');
                    cell.className='cell8';
                    console.log(`x: ${x}   y:${y} \n ${mapMesh[y][x]}`);
                    break;
            }
        }
    }
}
