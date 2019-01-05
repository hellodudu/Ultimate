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

type MUW_RequestPlayerInfo struct {
	MinLevel             int32    `protobuf:"varint,1,opt,name=min_level,json=minLevel,proto3" json:"min_level,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MUW_RequestPlayerInfo) Reset()         { *m = MUW_RequestPlayerInfo{} }
func (m *MUW_RequestPlayerInfo) String() string { return proto.CompactTextString(m) }
func (*MUW_RequestPlayerInfo) ProtoMessage()    {}
func (*MUW_RequestPlayerInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{7}
}

func (m *MUW_RequestPlayerInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MUW_RequestPlayerInfo.Unmarshal(m, b)
}
func (m *MUW_RequestPlayerInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MUW_RequestPlayerInfo.Marshal(b, m, deterministic)
}
func (m *MUW_RequestPlayerInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MUW_RequestPlayerInfo.Merge(m, src)
}
func (m *MUW_RequestPlayerInfo) XXX_Size() int {
	return xxx_messageInfo_MUW_RequestPlayerInfo.Size(m)
}
func (m *MUW_RequestPlayerInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MUW_RequestPlayerInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MUW_RequestPlayerInfo proto.InternalMessageInfo

func (m *MUW_RequestPlayerInfo) GetMinLevel() int32 {
	if m != nil {
		return m.MinLevel
	}
	return 0
}

type MWU_RequestPlayerInfo struct {
	Info                 []*MWU_RequestPlayerInfo_CrossPlayerInfo `protobuf:"bytes,1,rep,name=info,proto3" json:"info,omitempty"`
	XXX_NoUnkeyedLiteral struct{}                                 `json:"-"`
	XXX_unrecognized     []byte                                   `json:"-"`
	XXX_sizecache        int32                                    `json:"-"`
}

func (m *MWU_RequestPlayerInfo) Reset()         { *m = MWU_RequestPlayerInfo{} }
func (m *MWU_RequestPlayerInfo) String() string { return proto.CompactTextString(m) }
func (*MWU_RequestPlayerInfo) ProtoMessage()    {}
func (*MWU_RequestPlayerInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{8}
}

func (m *MWU_RequestPlayerInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_RequestPlayerInfo.Unmarshal(m, b)
}
func (m *MWU_RequestPlayerInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_RequestPlayerInfo.Marshal(b, m, deterministic)
}
func (m *MWU_RequestPlayerInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_RequestPlayerInfo.Merge(m, src)
}
func (m *MWU_RequestPlayerInfo) XXX_Size() int {
	return xxx_messageInfo_MWU_RequestPlayerInfo.Size(m)
}
func (m *MWU_RequestPlayerInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_RequestPlayerInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_RequestPlayerInfo proto.InternalMessageInfo

func (m *MWU_RequestPlayerInfo) GetInfo() []*MWU_RequestPlayerInfo_CrossPlayerInfo {
	if m != nil {
		return m.Info
	}
	return nil
}

type MWU_RequestPlayerInfo_CrossPlayerInfo struct {
	PlayerId             int64    `protobuf:"varint,1,opt,name=player_id,json=playerId,proto3" json:"player_id,omitempty"`
	UserId               string   `protobuf:"bytes,2,opt,name=user_id,json=userId,proto3" json:"user_id,omitempty"`
	ServerId             uint32   `protobuf:"varint,3,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	Gender               int32    `protobuf:"varint,4,opt,name=gender,proto3" json:"gender,omitempty"`
	Race                 int32    `protobuf:"varint,5,opt,name=race,proto3" json:"race,omitempty"`
	HeadProtrait         int32    `protobuf:"varint,6,opt,name=head_protrait,json=headProtrait,proto3" json:"head_protrait,omitempty"`
	HeadQuality          int32    `protobuf:"varint,7,opt,name=head_quality,json=headQuality,proto3" json:"head_quality,omitempty"`
	PlayerName           string   `protobuf:"bytes,8,opt,name=player_name,json=playerName,proto3" json:"player_name,omitempty"`
	GuildId              int64    `protobuf:"varint,9,opt,name=guild_id,json=guildId,proto3" json:"guild_id,omitempty"`
	LastLogoffTime       uint32   `protobuf:"varint,10,opt,name=last_logoff_time,json=lastLogoffTime,proto3" json:"last_logoff_time,omitempty"`
	Online               bool     `protobuf:"varint,11,opt,name=online,proto3" json:"online,omitempty"`
	Level                int32    `protobuf:"varint,12,opt,name=level,proto3" json:"level,omitempty"`
	VipLevel             int32    `protobuf:"varint,13,opt,name=vip_level,json=vipLevel,proto3" json:"vip_level,omitempty"`
	PlayerScore          int32    `protobuf:"varint,14,opt,name=player_score,json=playerScore,proto3" json:"player_score,omitempty"`
	HistoryScore         int32    `protobuf:"varint,15,opt,name=history_score,json=historyScore,proto3" json:"history_score,omitempty"`
	RemainsFloor         int32    `protobuf:"varint,16,opt,name=remains_floor,json=remainsFloor,proto3" json:"remains_floor,omitempty"`
	HeroTypeId           []uint32 `protobuf:"varint,17,rep,packed,name=hero_type_id,json=heroTypeId,proto3" json:"hero_type_id,omitempty"`
	FashionId            []uint32 `protobuf:"varint,18,rep,packed,name=fashion_id,json=fashionId,proto3" json:"fashion_id,omitempty"`
	MountTypeId          []uint32 `protobuf:"varint,19,rep,packed,name=mount_type_id,json=mountTypeId,proto3" json:"mount_type_id,omitempty"`
	RuneTypeId           []uint32 `protobuf:"varint,20,rep,packed,name=rune_type_id,json=runeTypeId,proto3" json:"rune_type_id,omitempty"`
	StateFlag            []int32  `protobuf:"varint,21,rep,packed,name=state_flag,json=stateFlag,proto3" json:"state_flag,omitempty"`
	PeakLevel            int32    `protobuf:"varint,22,opt,name=peak_level,json=peakLevel,proto3" json:"peak_level,omitempty"`
	PlayerStatus         uint32   `protobuf:"varint,23,opt,name=player_status,json=playerStatus,proto3" json:"player_status,omitempty"`
	CurDecorate          int32    `protobuf:"varint,24,opt,name=cur_decorate,json=curDecorate,proto3" json:"cur_decorate,omitempty"`
	ReturnPlayerId       int64    `protobuf:"varint,25,opt,name=return_player_id,json=returnPlayerId,proto3" json:"return_player_id,omitempty"`
	LastSquareActionTime uint32   `protobuf:"varint,26,opt,name=last_square_action_time,json=lastSquareActionTime,proto3" json:"last_square_action_time,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) Reset()         { *m = MWU_RequestPlayerInfo_CrossPlayerInfo{} }
func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) String() string { return proto.CompactTextString(m) }
func (*MWU_RequestPlayerInfo_CrossPlayerInfo) ProtoMessage()    {}
func (*MWU_RequestPlayerInfo_CrossPlayerInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{8, 0}
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo.Unmarshal(m, b)
}
func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo.Marshal(b, m, deterministic)
}
func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo.Merge(m, src)
}
func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) XXX_Size() int {
	return xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo.Size(m)
}
func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_RequestPlayerInfo_CrossPlayerInfo proto.InternalMessageInfo

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetPlayerId() int64 {
	if m != nil {
		return m.PlayerId
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetUserId() string {
	if m != nil {
		return m.UserId
	}
	return ""
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetServerId() uint32 {
	if m != nil {
		return m.ServerId
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetGender() int32 {
	if m != nil {
		return m.Gender
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetRace() int32 {
	if m != nil {
		return m.Race
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetHeadProtrait() int32 {
	if m != nil {
		return m.HeadProtrait
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetHeadQuality() int32 {
	if m != nil {
		return m.HeadQuality
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetPlayerName() string {
	if m != nil {
		return m.PlayerName
	}
	return ""
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetGuildId() int64 {
	if m != nil {
		return m.GuildId
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetLastLogoffTime() uint32 {
	if m != nil {
		return m.LastLogoffTime
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetOnline() bool {
	if m != nil {
		return m.Online
	}
	return false
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetLevel() int32 {
	if m != nil {
		return m.Level
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetVipLevel() int32 {
	if m != nil {
		return m.VipLevel
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetPlayerScore() int32 {
	if m != nil {
		return m.PlayerScore
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetHistoryScore() int32 {
	if m != nil {
		return m.HistoryScore
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetRemainsFloor() int32 {
	if m != nil {
		return m.RemainsFloor
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetHeroTypeId() []uint32 {
	if m != nil {
		return m.HeroTypeId
	}
	return nil
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetFashionId() []uint32 {
	if m != nil {
		return m.FashionId
	}
	return nil
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetMountTypeId() []uint32 {
	if m != nil {
		return m.MountTypeId
	}
	return nil
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetRuneTypeId() []uint32 {
	if m != nil {
		return m.RuneTypeId
	}
	return nil
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetStateFlag() []int32 {
	if m != nil {
		return m.StateFlag
	}
	return nil
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetPeakLevel() int32 {
	if m != nil {
		return m.PeakLevel
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetPlayerStatus() uint32 {
	if m != nil {
		return m.PlayerStatus
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetCurDecorate() int32 {
	if m != nil {
		return m.CurDecorate
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetReturnPlayerId() int64 {
	if m != nil {
		return m.ReturnPlayerId
	}
	return 0
}

func (m *MWU_RequestPlayerInfo_CrossPlayerInfo) GetLastSquareActionTime() uint32 {
	if m != nil {
		return m.LastSquareActionTime
	}
	return 0
}

type MUW_RequestGuildInfo struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MUW_RequestGuildInfo) Reset()         { *m = MUW_RequestGuildInfo{} }
func (m *MUW_RequestGuildInfo) String() string { return proto.CompactTextString(m) }
func (*MUW_RequestGuildInfo) ProtoMessage()    {}
func (*MUW_RequestGuildInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{9}
}

func (m *MUW_RequestGuildInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MUW_RequestGuildInfo.Unmarshal(m, b)
}
func (m *MUW_RequestGuildInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MUW_RequestGuildInfo.Marshal(b, m, deterministic)
}
func (m *MUW_RequestGuildInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MUW_RequestGuildInfo.Merge(m, src)
}
func (m *MUW_RequestGuildInfo) XXX_Size() int {
	return xxx_messageInfo_MUW_RequestGuildInfo.Size(m)
}
func (m *MUW_RequestGuildInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MUW_RequestGuildInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MUW_RequestGuildInfo proto.InternalMessageInfo

type MWU_RequestGuildInfo struct {
	Info                 []*MWU_RequestGuildInfo_CrossGuildInfo `protobuf:"bytes,2,rep,name=info,proto3" json:"info,omitempty"`
	XXX_NoUnkeyedLiteral struct{}                               `json:"-"`
	XXX_unrecognized     []byte                                 `json:"-"`
	XXX_sizecache        int32                                  `json:"-"`
}

func (m *MWU_RequestGuildInfo) Reset()         { *m = MWU_RequestGuildInfo{} }
func (m *MWU_RequestGuildInfo) String() string { return proto.CompactTextString(m) }
func (*MWU_RequestGuildInfo) ProtoMessage()    {}
func (*MWU_RequestGuildInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{10}
}

func (m *MWU_RequestGuildInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_RequestGuildInfo.Unmarshal(m, b)
}
func (m *MWU_RequestGuildInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_RequestGuildInfo.Marshal(b, m, deterministic)
}
func (m *MWU_RequestGuildInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_RequestGuildInfo.Merge(m, src)
}
func (m *MWU_RequestGuildInfo) XXX_Size() int {
	return xxx_messageInfo_MWU_RequestGuildInfo.Size(m)
}
func (m *MWU_RequestGuildInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_RequestGuildInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_RequestGuildInfo proto.InternalMessageInfo

func (m *MWU_RequestGuildInfo) GetInfo() []*MWU_RequestGuildInfo_CrossGuildInfo {
	if m != nil {
		return m.Info
	}
	return nil
}

type MWU_RequestGuildInfo_CrossGuildInfo struct {
	GuildId              int64    `protobuf:"varint,1,opt,name=guild_id,json=guildId,proto3" json:"guild_id,omitempty"`
	GuildName            string   `protobuf:"bytes,2,opt,name=guild_name,json=guildName,proto3" json:"guild_name,omitempty"`
	ServerId             uint32   `protobuf:"varint,3,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	MasterId             int64    `protobuf:"varint,4,opt,name=master_id,json=masterId,proto3" json:"master_id,omitempty"`
	MasterName           string   `protobuf:"bytes,5,opt,name=master_name,json=masterName,proto3" json:"master_name,omitempty"`
	CreateTime           uint32   `protobuf:"varint,6,opt,name=create_time,json=createTime,proto3" json:"create_time,omitempty"`
	HallLevel            int32    `protobuf:"varint,7,opt,name=hall_level,json=hallLevel,proto3" json:"hall_level,omitempty"`
	Score                int32    `protobuf:"varint,8,opt,name=score,proto3" json:"score,omitempty"`
	CastleScore          int32    `protobuf:"varint,9,opt,name=castle_score,json=castleScore,proto3" json:"castle_score,omitempty"`
	MemberNum            int32    `protobuf:"varint,10,opt,name=member_num,json=memberNum,proto3" json:"member_num,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) Reset()         { *m = MWU_RequestGuildInfo_CrossGuildInfo{} }
func (m *MWU_RequestGuildInfo_CrossGuildInfo) String() string { return proto.CompactTextString(m) }
func (*MWU_RequestGuildInfo_CrossGuildInfo) ProtoMessage()    {}
func (*MWU_RequestGuildInfo_CrossGuildInfo) Descriptor() ([]byte, []int) {
	return fileDescriptor_33865c398800497b, []int{10, 0}
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo.Unmarshal(m, b)
}
func (m *MWU_RequestGuildInfo_CrossGuildInfo) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo.Marshal(b, m, deterministic)
}
func (m *MWU_RequestGuildInfo_CrossGuildInfo) XXX_Merge(src proto.Message) {
	xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo.Merge(m, src)
}
func (m *MWU_RequestGuildInfo_CrossGuildInfo) XXX_Size() int {
	return xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo.Size(m)
}
func (m *MWU_RequestGuildInfo_CrossGuildInfo) XXX_DiscardUnknown() {
	xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo.DiscardUnknown(m)
}

var xxx_messageInfo_MWU_RequestGuildInfo_CrossGuildInfo proto.InternalMessageInfo

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetGuildId() int64 {
	if m != nil {
		return m.GuildId
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetGuildName() string {
	if m != nil {
		return m.GuildName
	}
	return ""
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetServerId() uint32 {
	if m != nil {
		return m.ServerId
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetMasterId() int64 {
	if m != nil {
		return m.MasterId
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetMasterName() string {
	if m != nil {
		return m.MasterName
	}
	return ""
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetCreateTime() uint32 {
	if m != nil {
		return m.CreateTime
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetHallLevel() int32 {
	if m != nil {
		return m.HallLevel
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetScore() int32 {
	if m != nil {
		return m.Score
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetCastleScore() int32 {
	if m != nil {
		return m.CastleScore
	}
	return 0
}

func (m *MWU_RequestGuildInfo_CrossGuildInfo) GetMemberNum() int32 {
	if m != nil {
		return m.MemberNum
	}
	return 0
}

func init() {
	proto.RegisterType((*MWU_WorldLogon)(nil), "world_message.MWU_WorldLogon")
	proto.RegisterType((*MUW_WorldLogon)(nil), "world_message.MUW_WorldLogon")
	proto.RegisterType((*MWU_TestConnect)(nil), "world_message.MWU_TestConnect")
	proto.RegisterType((*MUW_TestConnect)(nil), "world_message.MUW_TestConnect")
	proto.RegisterType((*MWU_HeartBeat)(nil), "world_message.MWU_HeartBeat")
	proto.RegisterType((*MUW_HeartBeat)(nil), "world_message.MUW_HeartBeat")
	proto.RegisterType((*MWU_WorldConnected)(nil), "world_message.MWU_WorldConnected")
	proto.RegisterType((*MUW_RequestPlayerInfo)(nil), "world_message.MUW_RequestPlayerInfo")
	proto.RegisterType((*MWU_RequestPlayerInfo)(nil), "world_message.MWU_RequestPlayerInfo")
	proto.RegisterType((*MWU_RequestPlayerInfo_CrossPlayerInfo)(nil), "world_message.MWU_RequestPlayerInfo.CrossPlayerInfo")
	proto.RegisterType((*MUW_RequestGuildInfo)(nil), "world_message.MUW_RequestGuildInfo")
	proto.RegisterType((*MWU_RequestGuildInfo)(nil), "world_message.MWU_RequestGuildInfo")
	proto.RegisterType((*MWU_RequestGuildInfo_CrossGuildInfo)(nil), "world_message.MWU_RequestGuildInfo.CrossGuildInfo")
}

func init() { proto.RegisterFile("world_message.proto", fileDescriptor_33865c398800497b) }

var fileDescriptor_33865c398800497b = []byte{
	// 854 bytes of a gzipped FileDescriptorProto
	0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0x7c, 0x95, 0x5f, 0x52, 0x1b, 0x47,
	0x10, 0xc6, 0x4b, 0x08, 0xfd, 0xd9, 0x16, 0x12, 0x78, 0x2c, 0x60, 0xed, 0x14, 0x65, 0x45, 0x7e,
	0xd1, 0x13, 0x49, 0x39, 0xce, 0x01, 0x12, 0xa7, 0x88, 0x95, 0xc2, 0x2e, 0xb2, 0x40, 0xf1, 0xb8,
	0x35, 0x68, 0x5b, 0x62, 0x2b, 0xbb, 0x3b, 0x62, 0x66, 0x96, 0x94, 0x2e, 0x93, 0x9b, 0xe4, 0x3d,
	0x07, 0xc9, 0x41, 0x52, 0xdd, 0x3d, 0x08, 0x89, 0x24, 0x7e, 0x53, 0xff, 0x7a, 0x76, 0x66, 0xba,
	0xfb, 0xfb, 0x46, 0xf0, 0xf2, 0x77, 0x63, 0x8b, 0x2c, 0x2d, 0xd1, 0x39, 0xbd, 0xc0, 0xd3, 0xa5,
	0x35, 0xde, 0xa8, 0xfe, 0x16, 0x1c, 0xff, 0x02, 0x83, 0x4f, 0x37, 0xd7, 0xe9, 0x0d, 0xc1, 0x73,
	0xb3, 0x30, 0x95, 0x7a, 0x05, 0x5d, 0x59, 0x92, 0x67, 0x71, 0x63, 0xd4, 0x98, 0xf4, 0x93, 0x0e,
	0xc7, 0xd3, 0x4c, 0x9d, 0x00, 0x48, 0xaa, 0xd2, 0x25, 0xc6, 0x3b, 0xa3, 0xc6, 0x24, 0x4a, 0x22,
	0x26, 0x9f, 0x75, 0x89, 0xe3, 0x03, 0x18, 0x7c, 0xba, 0xbe, 0xd9, 0xd8, 0x6b, 0xfc, 0x02, 0xf6,
	0x69, 0xf7, 0x2b, 0x74, 0xfe, 0x83, 0xa9, 0x2a, 0x9c, 0x79, 0x46, 0xd7, 0x37, 0x5b, 0x68, 0x1f,
	0xfa, 0xb4, 0xea, 0x23, 0x6a, 0xeb, 0x7f, 0x44, 0xed, 0xc7, 0xdf, 0x42, 0x9f, 0xd6, 0xac, 0x81,
	0x7a, 0x03, 0xbd, 0x5b, 0xed, 0x7d, 0x81, 0xa9, 0xcf, 0x4b, 0x0c, 0xd7, 0x02, 0x41, 0x57, 0x79,
	0x89, 0xe3, 0x6f, 0x40, 0xad, 0xcb, 0x08, 0xdb, 0x62, 0xf6, 0xac, 0x94, 0xe6, 0x46, 0x29, 0xe3,
	0xf7, 0x70, 0x48, 0x47, 0x24, 0x78, 0x5f, 0xa3, 0xf3, 0x17, 0x85, 0x5e, 0xa1, 0x9d, 0x56, 0x73,
	0xa3, 0xbe, 0x82, 0xa8, 0xcc, 0xab, 0xb4, 0xc0, 0x07, 0x2c, 0xf8, 0xa0, 0x56, 0xd2, 0x2d, 0xf3,
	0xea, 0x9c, 0xe2, 0xf1, 0x5f, 0x1d, 0x38, 0xa4, 0x73, 0xfe, 0xfd, 0xd9, 0x47, 0xd8, 0xcd, 0xab,
	0xb9, 0xe1, 0x63, 0x7a, 0xef, 0xde, 0x9f, 0x6e, 0xb7, 0xfe, 0x3f, 0xbf, 0x39, 0xfd, 0x60, 0x8d,
	0x73, 0x4f, 0x71, 0xc2, 0x3b, 0xbc, 0xfe, 0xbb, 0x0d, 0xfb, 0xcf, 0x32, 0x74, 0xa9, 0x25, 0x47,
	0x8f, 0x43, 0x69, 0x26, 0x5d, 0x01, 0xd3, 0x4c, 0x1d, 0x43, 0xa7, 0x76, 0x92, 0x92, 0x91, 0xb4,
	0x29, 0x9c, 0x66, 0xf4, 0x95, 0x43, 0xfb, 0x20, 0xa9, 0x26, 0xf7, 0xac, 0x2b, 0x60, 0x9a, 0xa9,
	0x23, 0x68, 0x2f, 0xb0, 0xca, 0xd0, 0xc6, 0xbb, 0x5c, 0x64, 0x88, 0x94, 0x82, 0x5d, 0xab, 0x67,
	0x18, 0xb7, 0x98, 0xf2, 0x6f, 0xf5, 0x16, 0xfa, 0x77, 0xa8, 0xb3, 0x94, 0x14, 0x64, 0x75, 0xee,
	0xe3, 0x36, 0x27, 0xf7, 0x08, 0x5e, 0x04, 0xa6, 0xbe, 0x06, 0x8e, 0xd3, 0xfb, 0x5a, 0x17, 0xb9,
	0x5f, 0xc5, 0x1d, 0x5e, 0xd3, 0x23, 0xf6, 0xab, 0x20, 0x1a, 0x63, 0x28, 0x83, 0x05, 0xd4, 0xe5,
	0xdb, 0x82, 0x20, 0x52, 0x10, 0x0d, 0x6c, 0x51, 0xe7, 0x32, 0xb0, 0x88, 0xcb, 0xec, 0x70, 0x3c,
	0xcd, 0xd4, 0x04, 0x0e, 0x0a, 0xed, 0x7c, 0x5a, 0x98, 0x85, 0x99, 0xcf, 0x45, 0x07, 0xc0, 0x35,
	0x0d, 0x88, 0x9f, 0x33, 0x26, 0x2d, 0x50, 0x65, 0xa6, 0x2a, 0xf2, 0x0a, 0xe3, 0xde, 0xa8, 0x31,
	0xe9, 0x26, 0x21, 0x52, 0x43, 0x68, 0xc9, 0x54, 0xf7, 0xf8, 0x66, 0x12, 0x50, 0x93, 0x1e, 0xf2,
	0x65, 0x98, 0x77, 0x5f, 0xe6, 0xfd, 0x90, 0x2f, 0x79, 0xde, 0x54, 0x53, 0xb8, 0xb0, 0x9b, 0x19,
	0x8b, 0xf1, 0x40, 0x6a, 0x12, 0x76, 0x49, 0x88, 0x7b, 0x93, 0x3b, 0x6f, 0xec, 0x2a, 0xac, 0xd9,
	0x0f, 0xbd, 0x11, 0xb8, 0x5e, 0x64, 0xb1, 0xd4, 0x79, 0xe5, 0xd2, 0x79, 0x61, 0x8c, 0x8d, 0x0f,
	0x64, 0x51, 0x80, 0x67, 0xc4, 0xd4, 0x88, 0x1a, 0x68, 0x4d, 0xea, 0x57, 0x4b, 0xa4, 0x06, 0xbc,
	0x60, 0xc5, 0x02, 0xb1, 0xab, 0xd5, 0x12, 0xc5, 0x7f, 0x73, 0xed, 0xee, 0x72, 0x53, 0x51, 0x5e,
	0x71, 0x3e, 0x0a, 0x64, 0x9a, 0xa9, 0x31, 0xf4, 0x4b, 0x53, 0x57, 0x7e, 0xbd, 0xc3, 0x4b, 0x5e,
	0xd1, 0x63, 0x18, 0xb6, 0x18, 0xc1, 0x9e, 0xad, 0x2b, 0x5c, 0x2f, 0x19, 0xca, 0x21, 0xc4, 0x9e,
	0x0e, 0x71, 0x5e, 0x7b, 0x4c, 0xe7, 0x85, 0x5e, 0xc4, 0x87, 0xa3, 0xe6, 0xa4, 0x95, 0x44, 0x4c,
	0xce, 0x0a, 0xbd, 0xa0, 0xf4, 0x12, 0xf5, 0x6f, 0xa1, 0x61, 0x47, 0x5c, 0x47, 0x44, 0x44, 0x3a,
	0xf6, 0x16, 0xfa, 0x8f, 0x1d, 0xf3, 0xda, 0xd7, 0x2e, 0x3e, 0xe6, 0x19, 0x85, 0x36, 0x5e, 0x32,
	0xa3, 0xb6, 0xce, 0x6a, 0x9b, 0x66, 0x38, 0x33, 0x56, 0x7b, 0x8c, 0x63, 0x69, 0xeb, 0xac, 0xb6,
	0x3f, 0x05, 0x44, 0xe3, 0xb6, 0xe8, 0x6b, 0x5b, 0xa5, 0x4f, 0xc2, 0x7f, 0xc5, 0x8a, 0x18, 0x08,
	0xbf, 0x78, 0x94, 0xff, 0xf7, 0x70, 0xcc, 0xc2, 0x70, 0xf7, 0xb5, 0xb6, 0x98, 0xea, 0x99, 0xa7,
	0xfe, 0xb0, 0x3e, 0x5e, 0xf3, 0xd9, 0x43, 0x4a, 0x5f, 0x72, 0xf6, 0x07, 0x4e, 0xf2, 0x8b, 0x71,
	0x04, 0xc3, 0x8d, 0x07, 0xe0, 0x67, 0x56, 0x59, 0x35, 0x37, 0xe3, 0x3f, 0x9a, 0x30, 0xdc, 0xb0,
	0xeb, 0x3a, 0xa1, 0xce, 0x82, 0xc3, 0x77, 0xd8, 0xe1, 0xef, 0xfe, 0xdf, 0xe1, 0xeb, 0x4f, 0xc4,
	0xe0, 0xeb, 0x30, 0xf8, 0xfb, 0xcf, 0x1d, 0x18, 0x6c, 0x27, 0xb6, 0x64, 0xdf, 0xd8, 0x96, 0xfd,
	0x09, 0x80, 0xa4, 0x36, 0x9f, 0x5c, 0x26, 0x6c, 0x98, 0x2f, 0x5a, 0x9c, 0x9e, 0x32, 0xed, 0xbc,
	0x24, 0x77, 0xe5, 0xd5, 0x10, 0x30, 0xcd, 0xc8, 0x8b, 0x21, 0xc9, 0x3b, 0xb7, 0xc4, 0x8b, 0x82,
	0x78, 0xeb, 0x37, 0xd0, 0x9b, 0x59, 0x24, 0x21, 0x70, 0x2f, 0xdb, 0xf2, 0xe6, 0x0a, 0x62, 0x9f,
	0x9d, 0x00, 0xdc, 0xe9, 0xa2, 0x08, 0x4a, 0x10, 0xbb, 0x47, 0x44, 0x44, 0x09, 0x43, 0x68, 0x89,
	0x21, 0xba, 0x62, 0x37, 0x0e, 0x78, 0xf4, 0xda, 0xd1, 0x4b, 0x2e, 0xc9, 0x28, 0x8c, 0x9e, 0x99,
	0x98, 0xe5, 0x04, 0xa0, 0xc4, 0xf2, 0x96, 0x6e, 0x56, 0x97, 0xec, 0xf1, 0x56, 0x12, 0x09, 0xf9,
	0x5c, 0x97, 0xb7, 0x6d, 0xfe, 0x1f, 0xfb, 0xee, 0x9f, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x25,
	0x94, 0xa3, 0xde, 0x06, 0x00, 0x00,
}
