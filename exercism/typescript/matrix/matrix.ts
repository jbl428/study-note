class Matrix {
    rows: number[][]
    columns: number[][]

    constructor(str: string) {
        this.rows = str.split('\n').map(r => r.split(' ').map(Number))
        this.columns = this.rows[0].map((_, index) => 
            this.rows.map(row => row[index])
        )
    }
}

export default Matrix
