<?php
/**
 * SuiteCRM Demo Suite Installation Helper
 * Copy this file to your SuiteCRM root directory and access it via browser
 */

// Check if we're in SuiteCRM directory
if (!file_exists('index.php') || !file_exists('config.php')) {
    die('❌ Error: This file must be placed in your SuiteCRM root directory');
}

session_start();
?>
<!DOCTYPE html>
<html>
<head>
    <title>SuiteCRM Demo Suite Installation Helper</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #534292;
            text-align: center;
        }
        .step {
            margin: 20px 0;
            padding: 20px;
            background: #f8f9fa;
            border-radius: 8px;
            border-left: 4px solid #534292;
        }
        .step h3 {
            margin-top: 0;
            color: #534292;
        }
        .button {
            display: inline-block;
            padding: 10px 20px;
            background: #534292;
            color: white;
            text-decoration: none;
            border-radius: 5px;
            margin: 5px;
        }
        .button:hover {
            background: #6a52b3;
        }
        .success {
            background: #d4edda;
            color: #155724;
            padding: 15px;
            border-radius: 5px;
            margin: 10px 0;
        }
        .warning {
            background: #fff3cd;
            color: #856404;
            padding: 15px;
            border-radius: 5px;
            margin: 10px 0;
        }
        .file-list {
            background: #f8f9fa;
            padding: 15px;
            border-radius: 5px;
            margin: 10px 0;
        }
        .file-list li {
            margin: 5px 0;
        }
        code {
            background: #e9ecef;
            padding: 2px 5px;
            border-radius: 3px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🚀 SuiteCRM Demo Suite Installation Helper</h1>
        
        <?php
        // Check if Module Loader is available
        if (!is_dir('modules/Administration') || !file_exists('modules/Administration/UpgradeWizard_prepare.php')) {
            echo '<div class="warning">⚠️ Module Loader may not be available. Ensure you have admin access.</div>';
        }
        ?>
        
        <div class="step">
            <h3>Step 1: Access Module Loader</h3>
            <p>Log in to SuiteCRM as an administrator and navigate to:</p>
            <p><strong>Admin → Developer Tools → Module Loader</strong></p>
            <a href="index.php?module=Administration&action=UpgradeWizard&view=module" class="button" target="_blank">
                Open Module Loader
            </a>
        </div>
        
        <div class="step">
            <h3>Step 2: Upload Demo Modules</h3>
            <p>Upload these files in the following order:</p>
            <div class="file-list">
                <ol>
                    <li><strong>DemoNavigator.zip</strong> - Provides navigation for all features</li>
                    <li><strong>DemoDataLoader.zip</strong> - Loads sample data</li>
                    <li><strong>Other modules</strong> - Install in any order:
                        <ul>
                            <li>SalesAnalyticsDashboard.zip</li>
                            <li>AILeadScoring.zip</li>
                            <li>InteractiveCustomerMap.zip</li>
                            <li>LiveChatIntegration.zip</li>
                            <li>MobileCRMDemo.zip</li>
                            <li>WorkflowAutomation.zip</li>
                        </ul>
                    </li>
                </ol>
            </div>
            <p><strong>Or</strong> upload <code>SuiteCRM_Demo_Suite_Complete.zip</code> for all modules at once.</p>
        </div>
        
        <div class="step">
            <h3>Step 3: Load Demo Data</h3>
            <p>After installing the modules:</p>
            <ol>
                <li>Navigate to <strong>Admin → Demo Data Loader</strong></li>
                <li>Click the "Load Demo Data" button</li>
                <li>Wait for the progress to complete (creates 500+ records)</li>
            </ol>
            <a href="index.php?module=Administration&action=DemoLoader" class="button" target="_blank">
                Open Demo Data Loader
            </a>
        </div>
        
        <div class="step">
            <h3>Step 4: Access Demo Features</h3>
            <p>Once data is loaded, explore the features:</p>
            <ul>
                <li><strong>Demo Navigator</strong> - Central hub for all demos</li>
                <li><strong>Sales Analytics</strong> - Add dashlet to home dashboard</li>
                <li><strong>AI Lead Scoring</strong> - Go to Leads menu</li>
                <li><strong>Customer Map</strong> - Go to Accounts menu</li>
                <li><strong>Live Chat</strong> - Add dashlet to dashboard</li>
            </ul>
            <a href="index.php?module=Home&action=DemoNavigator" class="button" target="_blank">
                Open Demo Navigator
            </a>
        </div>
        
        <div class="step">
            <h3>Quick Links</h3>
            <p>After installation, use these direct links:</p>
            <a href="index.php?module=Administration&action=DemoLoader" class="button">Load Demo Data</a>
            <a href="index.php?module=Home&action=DemoNavigator" class="button">Demo Navigator</a>
            <a href="index.php?module=Leads&action=AILeadScoring" class="button">AI Lead Scoring</a>
            <a href="index.php?module=Accounts&action=CustomerMap" class="button">Customer Map</a>
            <a href="index.php?module=Home&action=index" class="button">Dashboard</a>
        </div>
        
        <div class="success">
            💡 <strong>Tip:</strong> After installation, a floating demo button (🎯) will appear on all pages for quick access to the Demo Navigator.
        </div>
    </div>
</body>
</html>