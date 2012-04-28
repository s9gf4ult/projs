var MyApp = angular.module('MyApp', ['ngCookies']);

MyApp.config(function($routeProvider, $locationProvider) {
    $routeProvider
        .when('/activity/:name', {template: 'activity.html', controller: activityController})
        .when('/project/:name', {template : 'project.html', controller: projectController})
        .when('/', {template : 'main.html', controller: mainController})
        .otherwise({redirectTo: '/'});
    // $locationProvider.html5Mode(true);
});

var mainController = function($scope) {
    
};

var projectController = function($scope, $route, $routeParams) {
    $scope.name = $routeParams.name;
    $scope.reload = $route.reload;
}

var activityController = function($scope, $routeParams) {
    $scope.name = $routeParams.name;
}

var injector = angular.injector(['ng', 'MyApp']);
