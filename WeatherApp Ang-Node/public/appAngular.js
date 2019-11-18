var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope,$http) {
   $scope.submit= function(){
      console.log("app")
      var data = JSON.stringify({
            cityInput: $scope.cityName
        })
      console.log(data);
      $http.post("/city", data).then(function(data, status) {
         console.log(data);
        console.log('Data posted successfully');
        document.write(data.data);
      }).catch((err)=>{
          console.log("error");
          console.log(err);
      })
   }

});
