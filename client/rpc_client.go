package main

const (
	address  = "localhost:7080"
	playerID = []int64{
		1,
		2,
	}
)

func main() {

	// gameCli := pbGame.NewGameServiceClient("", nil)
	// Set up a connection to the server.
	// conn, err := grpc.Dial(address, grpc.WithInsecure())
	// if err != nil {
	// 	log.Fatalf("did not connect: %v", err)
	// }
	// defer conn.Close()
	// c := pb.NewGreeterClient(conn)

	// // Contact the server and print out its response.
	// name := defaultName
	// if len(os.Args) > 1 {
	// 	name = os.Args[1]
	// }
	// ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	// defer cancel()
	// r, err := c.SayHello(ctx, &pb.HelloRequest{Name: name})
	// if err != nil {
	// 	log.Fatalf("could not greet: %v", err)
	// }
	// log.Printf("Greeting: %s", r.Message)

	// i := pb.NewInviterClient(conn)
	// nctx, ncancel := context.WithTimeout(context.Background(), time.Second)
	// defer ncancel()
	// ir, err := i.GetScore(nctx, &pb.GetScoreRequest{Id: 1201616684167725058})
	// if err != nil {
	// 	log.Fatalf("could not invite: %v", err)
	// }
	// log.Printf("Inviter: %d", ir.Score)
}
