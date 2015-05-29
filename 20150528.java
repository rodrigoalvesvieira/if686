import java.util.Random;

/*
	O número de threads é definido pela linha de comando. Assim como o número de
	linhas e colunas de cada matriz (N, X, Y, Z) -> (X x Y) | (Y x Z)
	Logo, args[0] = N, args[1] = X, args[2] = Y e args[3] = Z.
*/

/*
	RESULTADOS PARA VARIAÇÕES DE THREADS E X
	N = 1, 2, 3, 4, 8, 16
	X = 10, 100, 1000, 10000, 100000

	X/N 	| 	1 	|	2 	| 	3 	| 	4 	| 	8 	| 	16	|	32	|	64
	------------------------------------------------------------------------
	10		|	3	|	3	|	3	|	4	|	5	|	6	|	10	|	14
	100		|	10	|	12	|	10	|	9	|	13	|	10	|	14	|	21
	1000	|	33	|	32	|	29	|	33	|	32	|	31	|	34	|	36
	10000	|	59	|	51	|	53	|	53	|	60	|	59	|	67	|	59
	100000	|	120	|	119	|	123	|	116	|	123	|	121	|	120	|	122
*/

public class 20150528 {

	public static void main(String[] args) {
		long time = System.currentTimeMillis(); // get current time

		int N = Integer.parseInt(args[0]), X = Integer.parseInt(args[1]), Y = 10, Z = 10, cont = 0;
		Thread[] threads = new Thread[N];
		Matriz A = new Matriz(X,Y);
		Matriz B = new Matriz(Y,Z);
		Matriz R = new Matriz(X,Z);
		Notify n = new Notify();

		A.generate(); B.generate();
		n.task = new int[2][X*Z];

		for(int i = 0 ; i< X; i++){
			for(int j = 0; j< Z; j++){
				n.task[0][cont] = i;
				n.task[1][cont] = j;
				cont++;
			}
		}

		for(int i = 0; i < N; i++){
			threads[i] = new Thread(new Multiply(A, B, R, n, X*Z));
			threads[i].start();

			try {
				threads[i].join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		System.out.println(System.currentTimeMillis() - time);
	}
}

class Matriz {
	int[][] matriz;

	public Matriz(int r, int c){
		matriz = new int[r][c];
	}

	public void generate(){
		Random randomGenerator = new Random();

		for(int i = 0; i < matriz.length; i++){
			for(int j = 0; j < matriz[0].length; j++){
				matriz[i][j] = randomGenerator.nextInt(10);
			}
		}
	}

	public void setAt(int i, int j,int v){
		matriz[i][j] = v;
	}

	public int getAt(int i, int j){
		return matriz[i][j];
	}
}

class Multiply implements Runnable {
	Matriz A;
	Matriz B;
	Matriz R;
	Notify n;
	int r, c, id;

	public Multiply(Matriz A, Matriz B, Matriz R, Notify n, int sum) {
		this.A = A;
		this.B = B;
		this.R = R;
		this.n = n;
		this.id = sum;
	}

	@Override
	public void run() {
		int att = 0;

		synchronized (n) {
			att = n.cont;
			if(n.cont >= id)
			 	return;

			c = n.task[1][att];
			r = n.task[0][att];
			n.cont++;
		}

		while(att < id){
			int val = 0, len = B.matriz.length;

			for(int k = 0; k < len; k++){
				val += A.getAt(r, k) * B.getAt(k, c);

			}

			R.setAt(r, c, val);

			synchronized (n) {
				att = n.cont;
				if(n.cont >= id)
					return;

				c = n.task[1][att];
				r = n.task[0][att];
				n.cont++;
			}
		}
	}
}

class Notify {
	int id, cont;
	int[][] task;

	public Notify() {
		cont = 0;
	}
}
