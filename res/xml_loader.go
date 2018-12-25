package res

import (
	"encoding/xml"
	"io/ioutil"
	"log"
	"os"
)

type XmlEntityClient struct {
	Id       uint32 `xml:"id"`
	Name     string `xml:"name"`
	Icon     string `xml:"icon"`
	Model    string `xml:"model"`
	Aptitude int32  `xml:"aptitude"`
}

type XmlLoader struct {
	EntityClientXml []XmlEntityClient
}

func NewXmlLoader() *XmlLoader {
	xmlLoader := &XmlLoader{}
	xmlFile, err := os.Open("entity_client.xml")
	if err != nil {
		log.Println(err)
	}
	defer xmlFile.Close()

	byXml, err := ioutil.ReadAll(xmlFile)
	if err != nil {
		log.Println(err)
	}

	var xmlData XmlEntityClient
	if err := xml.Unmarshal(byXml, &xmlData); err != nil {
		log.Println(err)
	}

	log.Printf("Unmarshal xml success : +%v\n", xmlData)

	return xmlLoader
}
