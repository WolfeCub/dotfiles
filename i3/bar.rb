#!/usr/bin/env ruby

require 'rubygems'
require 'barr'

$pcounter = 0

class Battery < Barr::Block
  def initialize opts={}
    super
  end

  def update!
    @output = `acpi --battery`
    @percent = Regexp.new('.*?(\\d+)(%)',Regexp::IGNORECASE).match(@output)[1]
    percent = @percent.to_i
    @time = Regexp.new('(\\d+)(.)(\\d+)',Regexp::IGNORECASE).match(@output).to_s
    @charging = `acpi --battery | grep "Charging"`.length > 1 ? true : false

    # Background color
    case percent
    when 90 .. 100
      @bgcolor = "#009f2d"
    when 80 .. 89
      @bgcolor = "#189228"
    when 70 .. 79
      @bgcolor = "#2F8523"
    when 60 .. 69
      @bgcolor = "#47781E"
    when 50 .. 59
      @bgcolor = "#5E6B19"
    when 40 .. 49
      @bgcolor = "#765D15"
    when 30 .. 39
      @bgcolor = "#8D5010"
    when 20 .. 29
      @bgcolor = "#A5430B"
    when 10 .. 19
      @bgcolor = "#BC3606"
    else
      @bgcolor = "#D42901"
    end

    # Icons
    if percent >= 90
      @icon = "\uf240"
    elsif percent >= 60
      @icon = "\uf241"
    elsif percent >= 40
      @icon = "\uf242"
    elsif percent > 10
      @icon = "\uf243"
    else
      @icon = "\uf244"
    end

    if percent <= 10 and ($pcounter % 24) == 0
      %x{notify-send "Battery at 10%"}
    elsif percent <= 5 and ($pcounter % 12) == 0
      %x{notify-send "Battery at 5%"}
    elsif percent <= 3 and ($pcounter % 12) == 0
      %x{notify-send "Battery at 3%"}
    end

    if @charging
        @icon = "\uf0e7"
    end

    $pcounter += 1

    @output = @percent + '% (' + @time + ')'
  end
end

class Volume < Barr::Block
    def initialize opts={}  # Don't forget to accept your options hash!
      # super ensures the common configurable options can be set
      super
    end

    def update!
      @stat = %x(amixer get Master | tail -n 1 | cut -d '[' -f 3 | sed 's/]//g')
      @cmd = %x(amixer get Master |grep % |awk '{print $5}'|sed 's/[^0-9]//g' |head -1)

      if @stat == "on\n"
          @icon = "\uf028"
      else
          @icon = "\uf026"
      end

      @bgcolor = "#a12c2c"
      @output = @cmd + "%"
    end
end

class Brightness < Barr::Block
    def initialize opts={}  # Don't forget to accept your options hash!
      # super ensures the common configurable options can be set
      super
    end

    def update!
      @cmd = %x(xbacklight)

      @icon = "\uf185"
      @bgcolor = "#d8c825"
      @output = @cmd.to_i.to_s + "%"
    end

end


@man = Barr::Manager.new

i3 = Barr::Blocks::I3.new(fgcolor: '#FFF',
                          bgcolor: '#145266',
                          focus_markers: %w(| |),
                          interval: 0.25,
                          icon: "\uf009")

clock = Barr::Blocks::Clock.new(bgcolor: '#371E5E',
                                format: '%r - %d %b %Y',
                                icon: "\uf073",
                                align: :c)

weather = Barr::Blocks::Temperature.new(bgcolor: '#4A072B',
                                        align: :l,
                                        location: '22664159',
                                        icon: "\uf0c2",
                                        interval: 1500)

hdd = Barr::Blocks::HDD.new bgcolor: '#444444', device: 'sda5', interval: 300

local = Barr::Blocks::IP.new device: 'wlp8s0', bgcolor: '#937739', align: :r, icon: "\uf1ce"

bat = Battery.new align: :r

vol = Volume.new interval: 1, align: :r

bright = Brightness.new interval: 1, align: :r

# Left
@man.add i3
@man.add weather
@man.add hdd

# Right
@man.add bright
@man.add vol
@man.add local
@man.add bat

# Center
@man.add clock

@man.run!
