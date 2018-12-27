package ultimate

import (
	"log"

	"github.com/golang/protobuf/proto"
)

func HandleRecvAddressBook(p proto.Message) {
	log.Printf("handleAddressBook:%v\n", p)
}
