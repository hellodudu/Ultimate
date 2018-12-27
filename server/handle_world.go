package ultimate

import (
	"log"

	"github.com/golang/protobuf/proto"
)

func handleAddressBook(p proto.Message) {
	log.Printf("handleAddressBook:%v\n", p)
}
