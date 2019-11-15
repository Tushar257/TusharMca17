var express = require('express'); 
var neode = require('neode');
const instance = new neode('bolt://localhost:7687','WeatherAppDatabase', '1234');
instance.model('WeatherDetails' , {
    cityName : {
        type : 'string'
    },
    tempToday :
    {
        type : 'string'
    },
    tempMinToday :
    {
        type : 'string'
    },
    tempMaxToday :
    {
        type : 'string'
    },
    tempMaxTom :{
        type : 'string'
    },
    tempMinTom : {
        type : 'string'
    },

    tempTom :{
        type : 'string'
    }
});
//instance.create('WeatherDetails', {






})
var axios = require("axios");
var app = express();
var bodyParser =require('body-parser');
app.use(bodyParser.urlencoded({'extended':'true'}));
app.use(bodyParser.json())
app.use(express.static(__dirname + '/public'));
app.get('/',(req,res)=>{
    res.sendFile("index.html");
})
app.post('/',(req,res)=>{
    console.log("received");
    console.log(req.body);
    res.send("ok")
})

app.post("/city", function( req, res ) {
    var city = req.body.cityInput;
    
    //axios.get("https://api.openweathermap.org/data/2.5/weather?q="+city+"&appid=31e69808cd436821a5e2fba6f494b787&units=metric")
    var resp = axios.get("https://api.openweathermap.org/data/2.5/forecast?q="+city+"&appid=31e69808cd436821a5e2fba6f494b787&units=metric")
    .then(response => {
        //res.render("cityweather", { weather : response.data, city : city } );
        console.log(response.data);
        res.send(response.data);
    })
    .catch(error => {
        console.log(error);
        //res.render( "cities", {merror : true} );
    });





});
app.listen(8080,()=>{
    console.log("Server Listen at port number : 8080");
});
