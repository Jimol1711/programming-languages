class Main{
    
    static int getNum5(int x){
        return 5;
    }
    static int getHii7(){
        System.out.println("Hii");
        return 7;
    }
    public static void main(String[] args) {
        System.out.println(getNum5(getHii7()));
    }
}
