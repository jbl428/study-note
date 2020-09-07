const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
const allNames: string[] = [];

export default class RobotName {
    name: string;

    constructor() {
         this.name = this.resetName()
    }

    resetName() : string{
        let temp: string
        do {
            temp = this.randomAlpa() + this.randomAlpa() + Math.floor( Math.random() * 1000);
        } while (allNames.includes(temp))
        this.name = temp;
        allNames.push(this.name)

        return this.name;
    }

    private randomAlpa(): string {
        return alphabet[Math.floor(Math.random() * alphabet.length)]
    }
}