<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2026 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

use PhpParser\Node;
use PhpParser\Node\Expr;
use PhpParser\Node\Expr\ArrayDimFetch;
use PhpParser\Node\Expr\Eval_;
use PhpParser\Node\Expr\Exit_;
use PhpParser\Node\Expr\FuncCall;
use PhpParser\Node\Expr\MethodCall;
use PhpParser\Node\Expr\New_;
use PhpParser\Node\Expr\PropertyFetch;
use PhpParser\Node\Expr\ShellExec;
use PhpParser\Node\Expr\StaticCall;
use PhpParser\Node\Expr\Variable;
use PhpParser\Node\Identifier;
use PhpParser\Node\Name;
use PhpParser\Node\Scalar\Int_;
use PhpParser\Node\Scalar\String_;
use PhpParser\Node\Stmt\Echo_;
use PhpParser\NodeVisitorAbstract;

/**
 * AST Node Visitor for ModuleScanner.
 *
 * Walks the parsed AST and detects:
 * - Direct calls to blacklisted functions: exec(), system(), etc.
 * - ALL dynamic/variable function calls: $f(), ($f)(), $_GET['cmd'](), $arr[0](), etc.
 *   These are inherently dangerous in untrusted code because the function name is determined
 *   at runtime, bypassing any static blacklist.
 * - Instantiation of blacklisted classes: new ReflectionClass(), etc.
 * - Calls to blacklisted static methods: SugarAutoLoader::put(), etc.
 * - Calls to blacklisted instance methods: $obj->setLevel(), etc.
 * - eval(), backtick shell execution, echo, exit/die
 */
class ModuleScannerVisitor extends NodeVisitorAbstract
{
    /** @var string[] */
    protected array $blackList;
    /** @var string[] */
    protected array $blackListExempt;
    /** @var string[] */
    protected array $classBlackList;
    /** @var string[] */
    protected array $classBlackListExempt;
    /** @var array */
    protected array $methodsBlackList;
    /** @var array */
    protected array $issues = [];

    public function __construct(
        array $blackList,
        array $blackListExempt,
        array $classBlackList,
        array $classBlackListExempt,
        array $methodsBlackList
    ) {
        $this->blackList = $blackList;
        $this->blackListExempt = $blackListExempt;
        $this->classBlackList = $classBlackList;
        $this->classBlackListExempt = $classBlackListExempt;
        $this->methodsBlackList = $methodsBlackList;
    }

    public function getIssues(): array
    {
        return $this->issues;
    }

    /**
     * @inheritDoc
     */
    public function enterNode(Node $node)
    {
        // --- Function calls: exec(), $f(), ($f)(), $_GET['cmd'](), etc. ---
        if ($node instanceof FuncCall) {
            $this->checkFuncCall($node);
            return;
        }

        // --- Static method calls: ClassName::method() ---
        if ($node instanceof StaticCall) {
            $this->checkStaticCall($node);
            return;
        }

        // --- Instance method calls: $obj->method() ---
        if ($node instanceof MethodCall) {
            $this->checkMethodCall($node);
            return;
        }

        // --- Class instantiation: new ClassName() ---
        if ($node instanceof New_) {
            $this->checkNewExpression($node);
            return;
        }

        // --- eval() ---
        if ($node instanceof Eval_) {
            if (in_array('eval', $this->blackList, true) && !in_array('eval', $this->blackListExempt, true)) {
                $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' eval()';
            }
            return;
        }

        // --- Backtick shell execution: `command` ---
        if ($node instanceof ShellExec) {
            $this->issues['backtick'] = translate('ML_INVALID_FUNCTION', 'Administration') . " '`'";
            return;
        }

        // --- exit / die ---
        if ($node instanceof Exit_) {
            $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' exit / die';
            return;
        }

        // --- echo ---
        if ($node instanceof Echo_) {
            $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' echo';
            return;
        }
    }

    /**
     * Check function calls.
     *
     * Handles two categories:
     * 1. Direct named calls (e.g. exec('cmd')) — checked against blacklist
     * 2. Dynamic/variable calls (e.g. $f(), ($f)(), $_GET['cmd']()) — always flagged
     *    because the function name is determined at runtime, making static blacklist
     *    checks ineffective
     */
    protected function checkFuncCall(FuncCall $node): void
    {
        if ($node->name instanceof Name) {
            // Direct named function call: exec(), system(), file_get_contents(), etc.
            $funcName = strtolower($node->name->toString());

            if (in_array($funcName, $this->blackList, true) && !in_array($funcName, $this->blackListExempt, true)) {
                $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' ' . $funcName . '()';
            }
            return;
        }

        // Dynamic/variable function call: $f(), ($f)(), $_GET['cmd'](), $arr[0](), etc.
        // The function name is an expression, not a static name. This means ANY function
        // could be called at runtime. Always flag these in untrusted code.
        $callDescription = $this->describeExpr($node->name);
        $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' ' . $callDescription . '()';
    }

    /**
     * Check static method calls against the class blacklist and methods blacklist.
     *
     * Detects patterns like:
     * - ReflectionClass::method() — blacklisted class
     * - SugarAutoLoader::put() — blacklisted method for specific class
     */
    protected function checkStaticCall(StaticCall $node): void
    {
        if (!($node->class instanceof Name)) {
            return;
        }

        $className = strtolower($node->class->toString());

        // Check class against class blacklist
        if (in_array($className, $this->classBlackList, true) && !in_array($className, $this->classBlackListExempt, true)) {
            $methodName = ($node->name instanceof Identifier) ? $node->name->toString() : '?';
            $this->issues[] = translate('ML_INVALID_METHOD', 'Administration') . ' ' . $className . '::' . $methodName . '()';
            return;
        }

        // Check method against methods blacklist (class-specific entries)
        if ($node->name instanceof Identifier) {
            $methodName = strtolower($node->name->toString());
            $this->checkMethodBlackList($methodName, $className, $className . '::' . $methodName);
        }
    }

    /**
     * Check instance method calls against the methods blacklist.
     *
     * Detects patterns like: $obj->setLevel(), $obj->put()
     */
    protected function checkMethodCall(MethodCall $node): void
    {
        if (!($node->name instanceof Identifier)) {
            return;
        }

        $methodName = strtolower($node->name->toString());
        $this->checkMethodBlackList($methodName, null, $methodName);
    }

    /**
     * Check a method name against the methods blacklist.
     *
     * The methodsBlackList has two forms:
     * - Numeric key (e.g. 0 => 'setlevel'): method is blacklisted globally
     * - String key with array value (e.g. 'put' => ['sugarautoloader']): method is
     *   blacklisted only for specific classes
     *
     * @param string $methodName Lowercase method name
     * @param string|null $className Lowercase class name (for static calls) or null (for instance calls)
     * @param string $displayName Display string for the issue message
     */
    protected function checkMethodBlackList(string $methodName, ?string $className, string $displayName): void
    {
        if (isset($this->methodsBlackList[$methodName])) {

            // Check class-specific method blacklist entries (string key => array of classes)
            if (is_array($this->methodsBlackList[$methodName]) && in_array($className ?? '', $this->methodsBlackList[$methodName], true)) {
                $this->issues[] = translate('ML_INVALID_METHOD', 'Administration') . ' ' . $displayName . '()';
                return;
            }

            if ($this->methodsBlackList[$methodName] === '*') {
                $this->issues[] = translate('ML_INVALID_METHOD', 'Administration') . ' ' . $displayName . '()';
                return;
            }
        }


        // Check globally blacklisted methods (numeric key)
        if (in_array($methodName, $this->methodsBlackList, true)) {
            $this->issues[] = translate('ML_INVALID_METHOD', 'Administration') . ' ' . $displayName . '()';
        }
    }

    /**
     * Check class instantiation against the class blacklist.
     *
     * Detects: new ReflectionClass(), new ZipArchive(), etc.
     */
    protected function checkNewExpression(New_ $node): void
    {
        if (!($node->class instanceof Name)) {
            return;
        }

        $className = strtolower($node->class->toString());

        if (in_array($className, $this->classBlackList, true) && !in_array($className, $this->classBlackListExempt, true)) {
            $this->issues[] = translate('ML_INVALID_FUNCTION', 'Administration') . ' new ' . $className . '()';
        }
    }

    /**
     * Produce a human-readable description of an expression for issue messages.
     *
     * Used for dynamic function calls where the name is an expression rather
     * than a static string (e.g. $f, $_GET['cmd'], $arr[0]).
     */
    protected function describeExpr(Expr $expr): string
    {
        if ($expr instanceof Variable) {
            if (is_string($expr->name)) {
                return '$' . $expr->name;
            }
            return '${}';
        }

        if ($expr instanceof ArrayDimFetch) {
            $var = $this->describeExpr($expr->var);
            if ($expr->dim instanceof String_) {
                return $var . '[\'' . $expr->dim->value . '\']';
            }
            if ($expr->dim instanceof Int_) {
                return $var . '[' . $expr->dim->value . ']';
            }
            return $var . '[...]';
        }

        if ($expr instanceof PropertyFetch) {
            $var = $this->describeExpr($expr->var);
            $prop = ($expr->name instanceof Identifier) ? $expr->name->toString() : '?';
            return $var . '->' . $prop;
        }

        return '{dynamic expression}';
    }
}