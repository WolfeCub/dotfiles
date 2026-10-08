// Caps Word (https://docs.qmk.fm/features/caps_word), assuming macOS Dvorak.
// Key codes are physical (QWERTY) positions. Karabiner runs this as ECMAScript 5.1.

var VAR = 'caps_word'
var TAP_VAR = 'caps_word_shift_tapped'
var TAP_MS = 300
var IDLE_MS = 5000

// Physical keys that produce letters under Dvorak
var LETTERS = [
  'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'semicolon',
  'x', 'c', 'v', 'b', 'n', 'm', 'comma', 'period', 'slash',
  'r', 't', 'y', 'u', 'i', 'o', 'p',
]
var MINUS = 'quote' // Dvorak "-"; shifted to "_"
var CONTINUE = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'delete_or_backspace', 'delete_forward']
// Everything else that should end the word
var OTHER = [
  'q', 'w', 'e', 'z', // ' , . ;
  'hyphen', 'equal_sign', 'open_bracket', 'close_bracket',
  'grave_accent_and_tilde', 'backslash', 'non_us_backslash',
  'spacebar', 'return_or_enter', 'tab', 'escape',
  'left_arrow', 'right_arrow', 'up_arrow', 'down_arrow',
  'home', 'end', 'page_up', 'page_down',
]

function is(name, value) {
  return { type: 'variable_if', name: name, value: value }
}

function set(name, value) {
  return { set_variable: { name: name, value: value } }
}

var ON = [set(VAR, 1), { set_notification_message: { id: VAR, text: 'CAPS WORD' } }]
var OFF = [set(VAR, 0), { set_notification_message: { id: VAR, text: '' } }]

function withIdleTimeout(m) {
  m.parameters = { 'basic.to_delayed_action_delay_milliseconds': IDLE_MS }
  m.to_delayed_action = { to_if_invoked: OFF }
  return m
}

// Double-tap a shift to toggle. The first tap acts as a normal shift and arms
// TAP_VAR, which is disarmed after TAP_MS or as soon as any other key is pressed.
function doubleTap(key) {
  var from = { key_code: key, modifiers: { optional: ['any'] } }
  return [
    {
      type: 'basic',
      conditions: [is(TAP_VAR, 1), is(VAR, 1)],
      from: from,
      to: [set(TAP_VAR, 0)].concat(OFF),
    },
    withIdleTimeout({
      type: 'basic',
      conditions: [is(TAP_VAR, 1)],
      from: from,
      to: [set(TAP_VAR, 0)].concat(ON),
    }),
    {
      type: 'basic',
      from: from,
      to: [set(TAP_VAR, 1), { key_code: key }],
      parameters: { 'basic.to_delayed_action_delay_milliseconds': TAP_MS },
      to_delayed_action: {
        to_if_invoked: [set(TAP_VAR, 0)],
        to_if_canceled: [set(TAP_VAR, 0)],
      },
    },
  ]
}

// Keep the word going, optionally shifting the key
function keep(shift) {
  return function (key) {
    var to = { key_code: key }
    if (shift) to.modifiers = ['left_shift']
    return withIdleTimeout({
      type: 'basic',
      conditions: [is(VAR, 1)],
      from: { key_code: key, modifiers: { optional: ['shift'] } },
      to: [to],
    })
  }
}

// End the word and pass the key through
function end(key) {
  return {
    type: 'basic',
    conditions: [is(VAR, 1)],
    from: { key_code: key, modifiers: { optional: ['any'] } },
    to: OFF.concat([{ key_code: key }]),
  }
}

function main() {
  return {
    description: 'Caps Word',
    description_notes: [
      '- Double-tap shift to toggle',
      '- Shifts letters and turns - into _ (Dvorak)',
      '- Digits and backspace keep it going',
      '- Any other key or 5s idle ends it',
    ],
    manipulators: [].concat(
      doubleTap('left_shift'),
      doubleTap('right_shift'),
      LETTERS.concat([MINUS]).map(keep(true)),
      CONTINUE.map(keep(false)),
      // Continue keys pressed with ctrl/opt/cmd fall through to here and end the word
      LETTERS.concat([MINUS], CONTINUE, OTHER).map(end)
    ),
  }
}

main()
