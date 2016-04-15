# Template Observer Pattern
# Date: 28-Jan-2016
# Authors:
#          A01165988 Azuri Gaytán Martínez
#          A01165792 Diego Monroy Fraustro

require 'observer'

class WeatherData 
  
  include Observable
  
  attr_reader :temperature, :humidity, :pressure, :temperature_min, :temperature_max, :temperature_sum, :num_of_temperatures, :pressure_prev
  # VARIABLES: temperature, humidity, pressure, global minimum temperature, global maximum temperature, sum of temperatures, previous pressure
  
  def initialize()
    @temperature = 0
    @humidity = 0
    @pressure = 0
    @temperature_min = 999999.0
    @temperature_max = 0.0
    @temperature_sum = 0.0
    @num_of_temperatures = 0.0
    @pressure_prev = 0
  end
  
  def set_measurements(temperature, humidity, pressure)
    # set the number of measurements +1
    @num_of_temperatures += 1.0
    # add current temperature to sum of temperatures
    @temperature_sum += temperature
    # get global maximum and minimum temperatures
    if temperature > @temperature_max
      @temperature_max = temperature
    end
    if temperature < @temperature_min
      @temperature_min = temperature
    end
    # if any measurement has changed, notify observers
    if temperature != @temperature || humidity != @humidity || pressure != @pressure
      @temperature = temperature
      @humidity = humidity
      @pressure = pressure
      changed
      notify_observers(self)
    end
    #give previous pressure the value of current pressure
    @pressure_prev = pressure
  end
  
end

class CurrentConditionsDisplay
  
  def update(data)
    # Display current measurements
    puts "Current conditions: #{data.temperature}°F and #{data.humidity}% humidity"
  end
  
end

class StatisticsDisplay
  
  def update(data)
    # Display maximum, minimum and average temperatures
    puts "Avg/Max/Min temperature = #{data.temperature_sum/data.num_of_temperatures}/#{data.temperature_max}/#{data.temperature_min}"
  end
  
end

class ForecastDisplay
  
  def update(data)
    # Display the weather forecast
    if data.pressure > data.pressure_prev
      puts "Forecast: Improving weather on the way!"
    elsif data.pressure < data.pressure_prev
      puts "Forecast: Watch out for cooler, rainy weather"
    else
      puts "Forecast: More of the same"
    end
  end
  
end