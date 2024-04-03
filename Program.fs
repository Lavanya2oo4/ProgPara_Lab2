// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from Lavanya"




// SECTION A---------------------------------------------------


// Define the Model
type Coach = {
    Name: string
    FormerPlayer: bool
}

type Stats = {
    Wins: int
    Losses: int
}

type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

// Create a List of 5 Teams
let teams = [
    { Name = "Miami Heat"; Coach = { Name = "Erik Spoelstra"; FormerPlayer = true }; Stats = { Wins = 1475; Losses = 1328 } }
    { Name = "Utah Jazz"; Coach = { Name = "Will Hardy"; FormerPlayer = true }; Stats = { Wins = 2146; Losses = 1804 } }
    { Name = "Washington Wizards"; Coach = { Name = "Brian Keefe"; FormerPlayer = true }; Stats = { Wins = 2257; Losses = 2748 } }
    { Name = "LA Clippers"; Coach = { Name = "Tyronn Lue"; FormerPlayer = true }; Stats = { Wins = 1792; Losses = 2486 } }
    { Name = "Houston Rockets"; Coach = { Name = "Ime Udoka"; FormerPlayer = true }; Stats = { Wins = 2328; Losses = 2196 } }
]

let printData (team_: Team) =
    printfn "Team name: %s" team_.Name
    printfn "Coach: %s" team_.Coach.Name
    printfn "Former Player: %b" team_.Coach.FormerPlayer 
    printfn "Wins : %d" team_.Stats.Wins
    printfn "Losses : %d \n" team_.Stats.Losses

teams |> List.iter printData





// Create a list of successful teams by filtering the list we created. The criterion to be a successful team is:
// Number of wins must be greater than number of losses.

let successfulTeams = teams |> List.filter (fun t -> t.Stats.Wins > t.Stats.Losses)
successfulTeams |> List.iter (fun team -> printfn "Team: %s | Coach: %s" team.Name team.Coach.Name)





printfn "\n"

// Mapping the List
let successPercent =
    teams
    |> List.map (fun team ->
        let win = float team.Stats.Wins
        let loss = float team.Stats.Losses
        let successPercentage = (win / (win + loss)) * 100.0
        (team.Name, successPercentage))

successPercent
|> List.iter (fun (name, percentage) -> printfn "Team: %s Success Percentage - %.3f %%" name percentage)

printfn "\n"



// SECTION B-----------------------------------------------------

type Cuisine =
    | Korean
    | Turkish


type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let getBudget (activity : Activity) =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie Regular -> 12.0
    | Movie IMAX -> 17.0
    | Movie DBOX -> 20.0
    | Movie RegularWithSnacks -> 12.0 + 5.0
    | Movie IMAXWithSnacks -> 17.0 + 5.0
    | Movie DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant Korean -> 70.0
    | Restaurant Turkish -> 65.0
    | LongDrive (km, perKmPrice) -> float km * perKmPrice


let printBudget (activity : Activity) =
    let budget = getBudget activity
    printfn "Activity: %A, Budget: %.2f dollar" activity budget

let activities = [
    BoardGame; Chill; 
    Movie Regular; Movie IMAX; Movie DBOX; 
    Movie RegularWithSnacks; Movie IMAXWithSnacks; Movie DBOXWithSnacks;
    Restaurant Korean; Restaurant Turkish; 
    LongDrive (100, 3)
]

activities |> List.iter printBudget
