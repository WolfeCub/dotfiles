#!/usr/bin/env ruby

require 'rubygems'
require 'barr'

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

    if @charging
        @icon = "\uf0e7"
    end

    @output = @percent + '% (' + @time + ')'
  end
end

@man = Barr::Manager.new

who = Barr::Blocks::Whoami.new align: :r, icon: "\uf007"

i3 = Barr::Blocks::I3.new(fgcolor: '#FFF',
                          bgcolor: '#145266',
                          focus_markers: %w(> <),
                          interval: 1,
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

hdd = Barr::Blocks::HDD.new bgcolor: '#444444', device: 'sda5', interval: 300, align: :r

local = Barr::Blocks::IP.new device: 'wlp8s0', bgcolor: '#937739', align: :r, icon: "\uf1ce"

bat = Battery.new align: :r

# Left
@man.add i3
@man.add weather

# Right
@man.add hdd
@man.add local
@man.add who
@man.add bat

# Center
@man.add clock

@man.run!
