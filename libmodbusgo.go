package kalibratorgo

/*
	binding to libmodbus
*/

/*
#cgo LDFLAGS: -lmodbus-5 -Li:/MinGW/msys/1.0/local/bin
#cgo CFLAGS: -I i:/MinGW/msys/1.0/local/include
#include <modbus/modbus.h>
*/
import "C"

import (
	"unsafe"
)

type RtuConnection struct {
	ctx *C.struct__modbus
}

func NewRtuConnection(device string, baud int, parity byte, data_bit int, stop_bit int) *RtuConnection {
	cs := C.CString(device)
	defer C.free(unsafe.Pointer(cs))
	context := C.modbus_new_rtu(cs, C.int(baud), C.char(parity), C.int(data_bit), C.int(stop_bit))

	return &RtuConnection{context}
}

func (this *RtuConnection) modbus_free() {
	C.modbus_free(this.ctx)
}
