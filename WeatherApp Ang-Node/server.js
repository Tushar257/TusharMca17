var express = require('express'); 
var neode = require('neode');
const instance = new neode('bolt://localhost:7687','neo4j', '12345');
instance.model('WeatherDetails' , {
    cityName : {
        type : 'string'
    },
    tempToday :
    {
        type : 'float'
    },
    tempMinToday :
    {
        type : 'float'
    },
    tempMaxToday :
    {
        type : 'float'
    },
    tempMaxTom :{
        type : 'float'
    },
    tempMinTom : {
        type : 'float'
    },

    tempTom :{
        type : 'flaot'
    }
});
var axios = require("axios");
var app = express();
var bodyParser =require('body-parser');
app.use(bodyParser.urlencoded({'extended':'true'}));
app.use(bodyParser.json())
app.use(express.static(__dirname + '/public'));
app.set("view engine","ejs");
app.get('/',(req,res)=>{
    res.sendFile("index.html");
})
app.post('/',(req,res)=>{
    console.log("received");
    console.log(req.body);
    res.send("ok")
})

    
app.post("/city", function( req, res ) {
    var city = req.body.cityInput.toLowerCase();
   
        axios.get("https://api.openweathermap.org/data/2.5/forecast?q="+city+"&appid=31e69808cd436821a5e2fba6f494b787&units=metric")
    .then(response => {
        
            instance.all('WeatherDetails', {cityName: city})
            .then(collection => {
            const length = collection.length
            if (length==0)
            {

            // INSERTING A NEW NODE IN THE DATABASE


            instance.create('WeatherDetails', {
            cityName :  city,
            tempToday: response.data.list[0].main.temp,
            tempMaxToday: response.data.list[0].main.temp_max,
            tempMinToday: response.data.list[0].main.temp_min,
            tempTom : response.data.list[6].main.temp,
            tempMaxTom : response.data.list[6].main.temp_max,
            tempMinTom: response.data.list[6].main.temp_min,
            
        
        }).then(cityName=>
            {   
            console.log("DATABASE UPDATED WITH " + city.toUpperCase() + "'s DATA")
            res.render("newcity",{weather : response.data , city:city});
            }).catch(e =>{
                console.log(e);
            });
        }

        else
        {   
            const citydata = collection.get(0).get('cityName')            
            const today = collection.get(0).get('tempToday')
            const todaymin = collection.get(0).get('tempMinToday')
            const todaymax = collection.get(0).get('tempMaxToday')
            const tom = collection.get(0).get('tempTom')
            const tommin = collection.get(0).get('tempMinTom')
            const tommax = collection.get(0).get('tempMaxTom')
            
            /*DELETION OF DATA
            instance.cypher('MATCH (n{cityName: {name}}) remove n.tempTom', {name: city})
            */


            /*UPDATION OF DATA
            instance.cypher("MATCH (n { cityName:{name}})SET n.tempTom=25.23" ,{name: city})
            */

            res.render("cityweather", { todaytemp : today, city : citydata, todaymin : todaymin, todaymax : todaymax, tom : tom, tommin: tommin ,tommax:tommax  } );
    
    
    
    }
    })
})

      .catch(error => {
        console.log(error);
    });

});

app.listen(8080,()=>{
    console.log("Server Listening at port number : 8080");
});
