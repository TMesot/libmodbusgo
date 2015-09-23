package kalibratorgo

import "testing"

func TestCall(t *testing.T) {
	context := NewRtuConnection("COM1", 112500, 'N', 8, 1)
	context.modbus_free()
}
