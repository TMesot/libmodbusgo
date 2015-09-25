package libmodbusgo

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
	"errors"
	"fmt"
	"syscall"
	"time"
	"unsafe"
)

const (
	INVALID_SLAVE = -1

	MODBUS_ERROR_RECOVERY_NONE     = 0
	MODBUS_ERROR_RECOVERY_LINK     = 1 << 0
	MODBUS_ERROR_RECOVERY_PROTOCOL = 1 << 1

	MODBUS_RTU_RTS_NONE = 0
	MODBUS_RTU_RTS_UP   = 1
	MODBUS_RTU_RTS_DOWN = 2
)

type RtuConnection struct {
	ctx         *C.struct__modbus
	curentSlave int16
	connected   bool
}

// Creates new ModbusRTU connection
func NewRtuConnection(device string, baud int, parity byte, data_bit uint8, stop_bit uint8) (*RtuConnection, error) {
	cs := C.CString(device)
	defer C.free(unsafe.Pointer(cs))
	context, errno := C.modbus_new_rtu(cs, C.int(baud), C.char(parity), C.int(data_bit), C.int(stop_bit))
	if context == nil {
		return nil, errors.New(fmt.Sprintf("Failed to create libmodbus context: %s", modbus_strerror(errno)))
	}

	return &RtuConnection{context, INVALID_SLAVE, false}, nil
}

// The modbus_free() function shall free an allocated modbus_t structure.
func (this *RtuConnection) Free() {
	if this.ctx != nil {
		C.modbus_free(this.ctx)
		this.ctx = nil
	} else {
		panic("Double free modbus context!")
	}
}

//The modbus_set_slave() function shall set the slave number in the libmodbus context.
//The behavior depends of network and the role of the device:
func (this *RtuConnection) Set_slave(slave uint8) error {
	res, errno := C.modbus_set_slave(this.ctx, C.int(slave))
	if int(res) != 0 {
		this.curentSlave = INVALID_SLAVE
		return errors.New(modbus_strerror(errno))
	} else {
		this.curentSlave = int16(slave)
		return nil
	}
}

// The modbus_connect() function shall etablish a connection to a Modbus server, a network or
// a bus using the context information of libmodbus context given in argument.
func (this *RtuConnection) Connect() error {
	res, errno := C.modbus_connect(this.ctx)
	if int(res) != 0 {
		this.connected = false
		return errors.New(fmt.Sprintf("Connection failed: %s", modbus_strerror(errno)))
	} else {
		this.connected = true
		return nil
	}
}

// The modbus_close() function shall close the connection established with the backend set in the context.
func (this *RtuConnection) Close() error {
	if this.connected {
		C.modbus_close(this.ctx)
		return nil
	} else {
		return errors.New("Not connected")
	}
}

// The modbus_flush() function shall discard data received but not read to the socket or file descriptor associated to the context ctx.
func (this *RtuConnection) Flush() error {
	res, errno := C.modbus_flush(this.ctx)
	if int(res) < 0 {
		return errors.New(modbus_strerror(errno))
	} else {
		return nil
	}
}

// The modbus_get_byte_timeout() function shall store the timeout interval between two consecutive bytes of the same message in the to_sec and to_usec arguments.
func (this *RtuConnection) Get_byte_timeout() (time.Duration, error) {
	var to_sec, to_usec C.uint32_t

	res, errno := C.modbus_get_byte_timeout(this.ctx, &to_sec, &to_usec)
	if res == 0 {
		return time.Duration(to_sec)*time.Second +
			time.Duration(to_usec)*time.Microsecond, nil
	} else {
		return 0, errors.New(modbus_strerror(errno))
	}
}

// The modbus_set_byte_timeout() function shall set the timeout interval between two consecutive bytes of the same message. The timeout is an upper bound on the amount of time elapsed before select() returns, if the time elapsed is longer than the defined timeout, an ETIMEDOUT error will be raised by the function waiting for a response.
//The value of to_usec argument must be in the range 0 to 999999.
//If both to_sec and to_usec are zero, this timeout will not be used at all. In this case, modbus_set_response_timeout() governs the entire handling of the response, the full confirmation response must be received before expiration of the response timeout. When a byte timeout is set, the response timeout is only used to wait for until the first byte of the response.
func (this *RtuConnection) Set_byte_timeout(timeout time.Duration) error {
	sec := timeout / time.Second

	to_sec := C.uint32_t(sec)
	to_usec := C.uint32_t((timeout - sec) / time.Microsecond)

	res, errno := C.modbus_set_byte_timeout(this.ctx, to_sec, to_usec)
	if int(res) != 0 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_get_response_timeout() function shall return the timeout interval used to wait for a response in the to_sec and to_usec arguments.
func (this *RtuConnection) Get_response_timeout() (time.Duration, error) {
	var to_sec, to_usec C.uint32_t

	res, errno := C.modbus_get_response_timeout(this.ctx, &to_sec, &to_usec)
	if int(res) == 0 {
		return time.Duration(to_sec)*time.Second +
			time.Duration(to_usec)*time.Microsecond, nil
	} else {
		return 0, errors.New(modbus_strerror(errno))
	}
}

// The modbus_set_response_timeout() function shall set the timeout interval used to wait for a response. When a byte timeout is set, if elapsed time for the first byte of response is longer than the given timeout, an ETIMEDOUT error will be raised by the function waiting for a response. When byte timeout is disabled, the full confirmation response must be received before expiration of the response timeout.
//The value of to_usec argument must be in the range 0 to 999999.
func (this *RtuConnection) Set_response_timeout(timeout time.Duration) error {
	sec := timeout / time.Second

	to_sec := C.uint32_t(sec)
	to_usec := C.uint32_t((timeout - sec) / time.Microsecond)

	res, errno := C.modbus_set_response_timeout(this.ctx, to_sec, to_usec)
	if int(res) != 0 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_set_error_recovery() function shall set the error recovery mode to apply when the connection fails or the byte received is not expected. The argument error_recovery may be bitwise-or’ed with zero or more of the following constants.
// By default there is no error recovery (MODBUS_ERROR_RECOVERY_NONE) so the application is responsible for controlling the error values returned by libmodbus functions and for handling them if necessary.
// When MODBUS_ERROR_RECOVERY_LINK is set, the library will attempt an reconnection after a delay defined by response timeout of the libmodbus context. This mode will try a infinite close/connect loop until success on send call and will just try one time to retablish the connection on select/read calls (if the connecton was down, the values to read are certainly not available anymore after reconnection, except for slave/server). This mode will also run flush requests after a delay based on the current response timeout in some situations (eg. timeout of select call). The reconnection attempt can hang for several seconds if the network to the remote target unit is down.
// When MODBUS_ERROR_RECOVERY_PROTOCOL is set, a sleep and flush sequence will be used to cleanup the ongoing communication, this can occurs when the message length is invalid, the TID is wrong or the received function code is not the expected one. The response timeout delay will be used to sleep.
// The modes are mask values and so they are complementary.
// It’s not recommended to enable error recovery for slave/server.
func (this *RtuConnection) Set_error_recovery(recovery_mode int) error {
	var mode C.modbus_error_recovery_mode = C.MODBUS_ERROR_RECOVERY_NONE

	if recovery_mode&MODBUS_ERROR_RECOVERY_LINK == MODBUS_ERROR_RECOVERY_LINK {
		mode &= C.MODBUS_ERROR_RECOVERY_LINK
	}
	if recovery_mode&MODBUS_ERROR_RECOVERY_PROTOCOL == MODBUS_ERROR_RECOVERY_PROTOCOL {
		mode &= C.MODBUS_ERROR_RECOVERY_PROTOCOL
	}

	res, errno := C.modbus_set_error_recovery(this.ctx, mode)
	if int(res) != 0 {
		return errors.New(modbus_strerror(errno))
	} else {
		return nil
	}
}

// The modbus_set_debug() function shall set the debug flag of the modbus_t context by using the argument flag. By default, the boolean flag is set to FALSE. When the flag value is set to TRUE, many verbose messages are displayed on stdout and stderr. For example, this flag is useful to display the bytes of the Modbus messages.
func (this *RtuConnection) Set_debug(debug bool) {
	var flag C.int = C.FALSE
	if debug {
		flag = C.TRUE
	}
	C.modbus_set_debug(this.ctx, flag)
}

// The modbus_read_bits() function shall read the status of the nb bits (coils) to the address addr of the remote device. The result of reading is stored in dest array as unsigned bytes (8 bits) set to TRUE or FALSE.
// You must take care to allocate enough memory to store the results in dest (at least nb * sizeof(uint8_t)).
// The function uses the Modbus function code 0x01 (read coil status).
func (this *RtuConnection) Read_bits(addr int, nb int) ([]bool, error) {
	ioresult := make([]C.uint8_t, nb)

	res, errno := C.modbus_read_bits(this.ctx, C.int(addr), C.int(nb), &ioresult[0])
	if int(res) == nb {
		bits := make([]bool, res)
		for i := 0; i < int(res); i++ {
			if ioresult[i] == C.TRUE {
				bits[i] = true
			}
		}
		return bits, nil
	} else {
		return nil, errors.New(modbus_strerror(errno))
	}
}

// The modbus_read_input_bits() function shall read the content of the nb input bits to the address addr of the remote device. The result of reading is stored in dest array as unsigned bytes (8 bits) set to TRUE or FALSE.
// You must take care to allocate enough memory to store the results in dest (at least nb * sizeof(uint8_t)).
// The function uses the Modbus function code 0x02 (read input status).
func (this *RtuConnection) Read_input_bits(addr int, nb int) ([]bool, error) {
	ioresult := make([]C.uint8_t, nb)

	res, errno := C.modbus_read_input_bits(this.ctx, C.int(addr), C.int(nb), &ioresult[0])
	if int(res) == nb {
		bits := make([]bool, int(res))
		for i := 0; i < int(res); i++ {
			if ioresult[i] == C.TRUE {
				bits[i] = true
			}
		}
		return bits, nil
	} else {
		return nil, errors.New(modbus_strerror(errno))
	}
}

// The modbus_read_registers() function shall read the content of the nb holding registers to the address addr of the remote device. The result of reading is stored in dest array as word values (16 bits).
// You must take care to allocate enough memory to store the results in dest (at least nb * sizeof(uint16_t)).
// The function uses the Modbus function code 0x03 (read holding registers).
func (this *RtuConnection) Read_registers(addr int, nb int) ([]uint16, error) {
	ioresult := make([]C.uint16_t, nb)

	res, errno := C.modbus_read_registers(this.ctx, C.int(addr), C.int(nb), &ioresult[0])
	if int(res) == nb {
		result := make([]uint16, int(res))
		for i := 0; i < int(res); i++ {
			result[i] = uint16(ioresult[i])
		}
		return result, nil
	} else {
		return nil, errors.New(modbus_strerror(errno))
	}
}

// The modbus_read_input_registers() function shall read the content of the nb input registers to address addr of the remote device. The result of the reading is stored in dest array as word values (16 bits).
// You must take care to allocate enough memory to store the results in dest (at least nb * sizeof(uint16_t)).
// The function uses the Modbus function code 0x04 (read input registers). The holding registers and input registers have different historical meaning, but nowadays it’s more common to use holding registers only.
func (this *RtuConnection) Read_input_registers(addr int, nb int) ([]uint16, error) {
	ioresult := make([]C.uint16_t, nb)

	res, errno := C.modbus_read_input_registers(this.ctx, C.int(addr), C.int(nb), &ioresult[0])
	if int(res) == nb {
		result := make([]uint16, int(res))
		for i := 0; i < int(res); i++ {
			result[i] = uint16(ioresult[i])
		}
		return result, nil
	} else {
		return nil, errors.New(modbus_strerror(errno))
	}
}

// The modbus_write_bit() function shall write the status of status at the address addr of the remote device. The value must be set to TRUE or FALSE.
// The function uses the Modbus function code 0x05 (force single coil).
func (this *RtuConnection) Write_bit(addr int, status bool) error {
	var val C.int = C.FALSE
	if status {
		val = C.TRUE
	}

	res, errno := C.modbus_write_bit(this.ctx, C.int(addr), val)
	if int(res) != 1 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_write_register() function shall write the value of value holding registers at the address addr of the remote device.
// The function uses the Modbus function code 0x06 (preset single register).
func (this *RtuConnection) Write_register(addr int, value uint16) error {
	res, errno := C.modbus_write_register(this.ctx, C.int(addr), C.int(value))
	if int(res) != 1 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_write_bits() function shall write the status of the nb bits (coils) from src at the address addr of the remote device. The src array must contains bytes set to TRUE or FALSE.
// The function uses the Modbus function code 0x0F (force multiple coils).
func (this *RtuConnection) Write_bits(addr int, values []bool) error {
	ioval := make([]C.uint8_t, len(values))
	for i := 0; i < len(values); i++ {
		ioval[i] = C.FALSE
		if values[i] {
			ioval[i] = C.TRUE
		}
	}

	res, errno := C.modbus_write_bits(this.ctx, C.int(addr), C.int(len(ioval)), &ioval[0])
	if int(res) != len(ioval) {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_write_registers() function shall write the content of the nb holding registers from the array src at address addr of the remote device.
// The function uses the Modbus function code 0x10 (preset multiple registers).
func (this *RtuConnection) Write_registers(addr int, values []uint16) error {
	ioval := make([]C.uint16_t, len(values))
	for i := 0; i < len(values); i++ {
		ioval[i] = C.uint16_t(values[i])
	}

	res, errno := C.modbus_write_registers(this.ctx, C.int(addr), C.int(len(ioval)), &ioval[0])
	if int(res) != len(ioval) {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The modbus_send_raw_request() function shall send a request via the socket of the context ctx. This function must be used for debugging purposes because you have to take care to make a valid request by hand. The function only adds to the message, the header or CRC of the selected backend, so raw_req must start and contain at least a slave/unit identifier and a function code. This function can be used to send request not handled by the library.
// The public header of libmodbus provides a list of supported Modbus functions codes, prefixed by MODBUS_FC_ (eg. MODBUS_FC_READ_HOLDING_REGISTERS), to help build of raw requests.
func (this *RtuConnection) Send_raw_request(request []byte) error {
	res, errno := C.modbus_send_raw_request(this.ctx, (*C.uint8_t)(unsafe.Pointer(&request[0])),
		C.int(len(request)))
	if res < 0 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

// The *modbus_receive_confirmation(*_ function shall receive a request via the socket of the context ctx. This function must be used for debugging purposes because the received response isn’t checked against the initial request. This function can be used to receive request not handled by the library.
func (this *RtuConnection) Receive_confirmation() ([]byte, error) {
	iodata := make([]C.uint8_t, 512)
	res, errno := C.modbus_receive_confirmation(this.ctx, &iodata[0])
	if int(res) > 0 {
		if int(res) == 0 {
			return nil, errors.New("No data ressived")
		}
		return nil, errors.New(modbus_strerror(errno))
	} else {
		result := make([]byte, int(res))
		for i := 0; i < int(res); i++ {
			result[i] = byte(iodata[i])
		}
		return result, nil
	}
}

// The modbus_rtu_get_rts() function shall get the current Request To Send mode of the libmodbus context ctx. The possible returned values are:
//    MODBUS_RTU_RTS_NONE
//    MODBUS_RTU_RTS_UP
//    MODBUS_RTU_RTS_DOWN
// This function can only be used with a context using a RTU backend.
func (this *RtuConnection) Rtu_get_rts() int {
	res := C.modbus_rtu_get_rts(this.ctx)
	switch res {
	case C.MODBUS_RTU_RTS_NONE:
		return MODBUS_RTU_RTS_NONE
	case C.MODBUS_RTU_RTS_UP:
		return MODBUS_RTU_RTS_UP
	case C.MODBUS_RTU_RTS_DOWN:
		return MODBUS_RTU_RTS_DOWN
	default:
		return -1
	}
}

// The modbus_rtu_set_rts() function shall set the Request To Send mode to communicate on a RS485 serial bus. By default, the mode is set to MODBUS_RTU_RTS_NONE and no signal is issued before writing data on the wire.
// To enable the RTS mode, the values MODBUS_RTU_RTS_UP or MODBUS_RTU_RTS_DOWN must be used, these modes enable the RTS mode and set the polarity at the same time. When MODBUS_RTU_RTS_UP is used, an ioctl call is made with RTS flag enabled then data is written on the bus after a delay of 1ms, then another ioctl call is made with the RTS flag disabled and again a delay of 1ms occurs. The MODBUS_RTU_RTS_DOWN mode applies the same procedure but with an inversed RTS flag.
// This function can only be used with a context using a RTU backend.
func (this *RtuConnection) Rtu_set_rts(mode int) error {
	var m C.int
	switch mode {
	case MODBUS_RTU_RTS_NONE:
		m = C.MODBUS_RTU_RTS_NONE
	case MODBUS_RTU_RTS_UP:
		m = C.MODBUS_RTU_RTS_UP
	case MODBUS_RTU_RTS_DOWN:
		m = C.MODBUS_RTU_RTS_DOWN
	default:
		return errors.New("Invalid mode")
	}
	res, errno := C.modbus_rtu_set_rts(this.ctx, m)
	if int(res) != 0 {
		return errors.New(modbus_strerror(errno))
	}
	return nil
}

func modbus_strerror(errno error) string {
	if val, ok := errno.(syscall.Errno); ok {
		return C.GoString(C.modbus_strerror(C.int(val)))
	} else {
		return "UNKNOWN ERROR while processing errno"
	}
}
