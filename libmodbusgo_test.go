package libmodbusgo

import (
	"fmt"
	"testing"
)

func TestCall(t *testing.T) {
	context, err := NewRtuConnection("COM1", 112500, 'N', 8, 1)
	if err != nil {
		t.Error(err.Error())
	}
	context.modbus_free()
}

func TestErrno(t *testing.T) {
	context, err := NewRtuConnection("", 112500, 'N', 8, 1)
	if err == nil {
		context.modbus_free()
		t.Error("Checking errno working: error")
	} else {
		t.Log(fmt.Sprintf("Checking errno working: pass (%s)", err.Error()))
	}
}
