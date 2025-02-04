import torch

def export_model(model, output_path="models/exported_model.pth"):
    """Exports the trained RL model to a file."""
    torch.save(model.state_dict(), output_path)
    print(f"Model exported to {output_path}")

if __name__ == "__main__":
    from solidity_rl.models.rl_policy import RLPolicy
    
    model = RLPolicy()
    export_model(model)
