import random

def generateSalesData(n):
    """Generate n sales records with deterministic random data"""
    random.seed(42)

    products = ["Widget", "Gadget", "Doohickey", "Thingamajig", "Gizmo"]
    categories = ["Electronics", "Hardware", "Software", "Services"]
    regions = ["North", "South", "East", "West"]

    sales = []
    for i in range(n):
        record = {
            "product": random.choice(products),
            "category": random.choice(categories),
            "quantity": random.randint(1, 100),
            "price": round(random.uniform(10.0, 500.0), 2),
            "region": random.choice(regions)
        }
        sales.append(record)

    return sales

def formatReport(summaries):
    """Format category summaries as a text report"""
    lines = ["Category Sales Report", "=" * 50]

    for summary in summaries:
        line = f"{summary['category']:20s} | Total: ${summary['totalSales']:10.2f} | Items: {summary['itemCount']:5d} | Avg: ${summary['avgPrice']:8.2f}"
        lines.append(line)

    lines.append("=" * 50)
    return "\n".join(lines)
