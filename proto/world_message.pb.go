// Code generated by protoc-gen-go. DO NOT EDIT.
// source: world_message.proto

package world_message

import (
	fmt "fmt"
	proto "github.com/golang/protobuf/proto"
	math "math"
)

// Reference imports to suppress errors if they are not otherwise used.
var _ = proto.Marshal
var _ = fmt.Errorf
var _ = math.Inf

// This is a compile-time assertion to ensure that this generated file
// is compatible with the proto package it is being compiled against.
// A compilation error at this line likely means your copy of the
// proto package needs to be updated.
const _ = proto.ProtoPackageIsVersion3 // please upgrade the proto package

type MWU_WorldLogon struct {
	WorldId              uint32   `protobuf:"varint,1,opt,name=world_id,json=worldId,proto3" json:"world_id,omitempty"`
	WorldName            string   `protobuf:"bytes,2,opt,name=world_name,json=worldName,proto3" json:"world_name,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_WorldLogon) Reset()         { *m = MWU_WorldLogon{} }
func (m *MWU_WorldLogon) String() string { return proto.CompactTextString(m) }
func (*MWU_WorldLogon) ProtoMessage()    {}
func (*MWU_WorldLogon) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{0}
}

func (m *MWU_WorldLogon) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_WorldLogon.Unmarshal(m, b)
}
func (m *MWU_WorldLogon) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_WorldLogon.Marshal(b, m, deterministic)
}
func (m *MWU_WorldLogon) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_WorldLogon.Merge(m, src)
}
func (m *MWU_WorldLogon) XXX_Size() int {
	return xxx_messageInfo_MWU_WorldLogon.Size(m)
}
func (m *MWU_WorldLogon) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_WorldLogon.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_WorldLogon proto.InternalMessageInfo

func (m *MWU_WorldLogon) GetWorldId() uint32 {
	if m != nil {
		return m.WorldId
	}
	return 0
}

func (m *MWU_WorldLogon) GetWorldName() string {
	if m != nil {
		return m.WorldName
	}
	return ""
}

type MUW_WorldLogon struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MUW_WorldLogon) Reset()         { *m = MUW_WorldLogon{} }
func (m *MUW_WorldLogon) String() string { return proto.CompactTextString(m) }
func (*MUW_WorldLogon) ProtoMessage()    {}
func (*MUW_WorldLogon) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{1}
}

func (m *MUW_WorldLogon) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MUW_WorldLogon.Unmarshal(m, b)
}
func (m *MUW_WorldLogon) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MUW_WorldLogon.Marshal(b, m, deterministic)
}
func (m *MUW_WorldLogon) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MUW_WorldLogon.Merge(m, src)
}
func (m *MUW_WorldLogon) XXX_Size() int {
	return xxx_messageInfo_MUW_WorldLogon.Size(m)
}
func (m *MUW_WorldLogon) XXX_DiscardUnknown() {
	xxx_messageInfo_MUW_WorldLogon.DiscardUnknown(m)
}

var xxx_messageInfo_MUW_WorldLogon proto.InternalMessageInfo

type MWU_TestConnect struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_TestConnect) Reset()         { *m = MWU_TestConnect{} }
func (m *MWU_TestConnect) String() string { return proto.CompactTextString(m) }
func (*MWU_TestConnect) ProtoMessage()    {}
func (*MWU_TestConnect) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{2}
}

func (m *MWU_TestConnect) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_TestConnect.Unmarshal(m, b)
}
func (m *MWU_TestConnect) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_TestConnect.Marshal(b, m, deterministic)
}
func (m *MWU_TestConnect) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_TestConnect.Merge(m, src)
}
func (m *MWU_TestConnect) XXX_Size() int {
	return xxx_messageInfo_MWU_TestConnect.Size(m)
}
func (m *MWU_TestConnect) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_TestConnect.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_TestConnect proto.InternalMessageInfo

type MUW_TestConnect struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MUW_TestConnect) Reset()         { *m = MUW_TestConnect{} }
func (m *MUW_TestConnect) String() string { return proto.CompactTextString(m) }
func (*MUW_TestConnect) ProtoMessage()    {}
func (*MUW_TestConnect) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{3}
}

func (m *MUW_TestConnect) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MUW_TestConnect.Unmarshal(m, b)
}
func (m *MUW_TestConnect) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MUW_TestConnect.Marshal(b, m, deterministic)
}
func (m *MUW_TestConnect) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MUW_TestConnect.Merge(m, src)
}
func (m *MUW_TestConnect) XXX_Size() int {
	return xxx_messageInfo_MUW_TestConnect.Size(m)
}
func (m *MUW_TestConnect) XXX_DiscardUnknown() {
	xxx_messageInfo_MUW_TestConnect.DiscardUnknown(m)
}

var xxx_messageInfo_MUW_TestConnect proto.InternalMessageInfo

type MWU_HeartBeat struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_HeartBeat) Reset()         { *m = MWU_HeartBeat{} }
func (m *MWU_HeartBeat) String() string { return proto.CompactTextString(m) }
func (*MWU_HeartBeat) ProtoMessage()    {}
func (*MWU_HeartBeat) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{4}
}

func (m *MWU_HeartBeat) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_HeartBeat.Unmarshal(m, b)
}
func (m *MWU_HeartBeat) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_HeartBeat.Marshal(b, m, deterministic)
}
func (m *MWU_HeartBeat) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_HeartBeat.Merge(m, src)
}
func (m *MWU_HeartBeat) XXX_Size() int {
	return xxx_messageInfo_MWU_HeartBeat.Size(m)
}
func (m *MWU_HeartBeat) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_HeartBeat.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_HeartBeat proto.InternalMessageInfo

type MUW_HeartBeat struct {
	BattleTime           uint32   `protobuf:"varint,1,opt,name=battle_time,json=battleTime,proto3" json:"battle_time,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MUW_HeartBeat) Reset()         { *m = MUW_HeartBeat{} }
func (m *MUW_HeartBeat) String() string { return proto.CompactTextString(m) }
func (*MUW_HeartBeat) ProtoMessage()    {}
func (*MUW_HeartBeat) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{5}
}

func (m *MUW_HeartBeat) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MUW_HeartBeat.Unmarshal(m, b)
}
func (m *MUW_HeartBeat) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MUW_HeartBeat.Marshal(b, m, deterministic)
}
func (m *MUW_HeartBeat) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MUW_HeartBeat.Merge(m, src)
}
func (m *MUW_HeartBeat) XXX_Size() int {
	return xxx_messageInfo_MUW_HeartBeat.Size(m)
}
func (m *MUW_HeartBeat) XXX_DiscardUnknown() {
	xxx_messageInfo_MUW_HeartBeat.DiscardUnknown(m)
}

var xxx_messageInfo_MUW_HeartBeat proto.InternalMessageInfo

func (m *MUW_HeartBeat) GetBattleTime() uint32 {
	if m != nil {
		return m.BattleTime
	}
	return 0
}

type MWU_WorldConnected struct {
	WorldId              []uint32 `protobuf:"varint,1,rep,packed,name=world_id,json=worldId,proto3" json:"world_id,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_WorldConnected) Reset()         { *m = MWU_WorldConnected{} }
func (m *MWU_WorldConnected) String() string { return proto.CompactTextString(m) }
func (*MWU_WorldConnected) ProtoMessage()    {}
func (*MWU_WorldConnected) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{6}
}

func (m *MWU_WorldConnected) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_WorldConnected.Unmarshal(m, b)
}
func (m *MWU_WorldConnected) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_WorldConnected.Marshal(b, m, deterministic)
}
func (m *MWU_WorldConnected) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_WorldConnected.Merge(m, src)
}
func (m *MWU_WorldConnected) XXX_Size() int {
	return xxx_messageInfo_MWU_WorldConnected.Size(m)
}
func (m *MWU_WorldConnected) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_WorldConnected.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_WorldConnected proto.InternalMessageInfo

func (m *MWU_WorldConnected) GetWorldId() []uint32 {
	if m != nil {
		return m.WorldId
	}
	return nil
}

func init() {
	proto.RegisterType((*MWU_WorldLogon)(nil), "world_message.MWU_WorldLogon")
	proto.RegisterType((*MUW_WorldLogon)(nil), "world_message.MUW_WorldLogon")
	proto.RegisterType((*MWU_TestConnect)(nil), "world_message.MWU_TestConnect")
	proto.RegisterType((*MUW_TestConnect)(nil), "world_message.MUW_TestConnect")
	proto.RegisterType((*MWU_HeartBeat)(nil), "world_message.MWU_HeartBeat")
	proto.RegisterType((*MUW_HeartBeat)(nil), "world_message.MUW_HeartBeat")
	proto.RegisterType((*MWU_WorldConnected)(nil), "world_message.MWU_WorldConnected")
}

func init() { proto.RegisterFile("world_message.proto", fileDescriptor_33865c398800497b) }

var fileDescriptor_33865c398800497b = []byte{
	// 205 bytes of a gzipped FileDescriptorProto
	0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0x5c, 0xd0, 0xc1, 0x4b, 0x80, 0x30,
	0x14, 0x06, 0x70, 0x2c, 0xa8, 0x7c, 0x31, 0xad, 0x75, 0xb1, 0x43, 0x24, 0x3b, 0x79, 0xaa, 0xa0,
	0xff, 0xa0, 0x2e, 0x15, 0xd9, 0x41, 0x94, 0x1d, 0x65, 0xb6, 0x87, 0x08, 0x6e, 0x0b, 0x7d, 0xd0,
	0xbf, 0x1f, 0x9b, 0x22, 0xda, 0x71, 0xbf, 0x7d, 0x7c, 0xec, 0x1b, 0xdc, 0xfc, 0xba, 0x69, 0xd4,
	0xad, 0xc1, 0x79, 0x56, 0x3d, 0x3e, 0xfc, 0x4c, 0x8e, 0x1c, 0x67, 0x07, 0x14, 0x1f, 0x90, 0x94,
	0xb2, 0x69, 0xa5, 0xc7, 0x4f, 0xd7, 0x3b, 0xcb, 0x6f, 0xe1, 0x62, 0x89, 0x0c, 0x3a, 0x8b, 0xf2,
	0xa8, 0x60, 0xd5, 0x79, 0x38, 0xbf, 0x6b, 0x7e, 0x07, 0xb0, 0x5c, 0x59, 0x65, 0x30, 0x3b, 0xc9,
	0xa3, 0x22, 0xae, 0xe2, 0x20, 0x5f, 0xca, 0xa0, 0xb8, 0x82, 0xa4, 0x6c, 0xe4, 0xae, 0x4b, 0x5c,
	0x43, 0xea, 0xdb, 0x6b, 0x9c, 0xe9, 0xd5, 0x59, 0x8b, 0xdf, 0x14, 0xa8, 0x91, 0x07, 0x4a, 0x81,
	0xf9, 0xd4, 0x1b, 0xaa, 0x89, 0x5e, 0x50, 0x91, 0x78, 0x02, 0xe6, 0x33, 0x1b, 0xf0, 0x7b, 0xb8,
	0xec, 0x14, 0xd1, 0x88, 0x2d, 0x0d, 0x06, 0xd7, 0x67, 0xc1, 0x42, 0xf5, 0x60, 0x50, 0x3c, 0x02,
	0xdf, 0x66, 0xac, 0xb5, 0xa8, 0xff, 0x4d, 0x39, 0xdd, 0x4d, 0xe9, 0xce, 0xc2, 0x6f, 0x3c, 0xff,
	0x05, 0x00, 0x00, 0xff, 0xff, 0x02, 0x32, 0xbf, 0x70, 0x24, 0x01, 0x00, 0x00,
}