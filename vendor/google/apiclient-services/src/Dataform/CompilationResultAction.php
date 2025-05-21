<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Dataform;

class CompilationResultAction extends \Google\Model
{
  protected $assertionType = Assertion::class;
  protected $assertionDataType = '';
  protected $canonicalTargetType = Target::class;
  protected $canonicalTargetDataType = '';
  protected $declarationType = Declaration::class;
  protected $declarationDataType = '';
  /**
   * @var string
   */
  public $filePath;
  /**
   * @var string
   */
  public $internalMetadata;
  protected $notebookType = Notebook::class;
  protected $notebookDataType = '';
  protected $operationsType = Operations::class;
  protected $operationsDataType = '';
  protected $relationType = Relation::class;
  protected $relationDataType = '';
  protected $targetType = Target::class;
  protected $targetDataType = '';

  /**
   * @param Assertion
   */
  public function setAssertion(Assertion $assertion)
  {
    $this->assertion = $assertion;
  }
  /**
   * @return Assertion
   */
  public function getAssertion()
  {
    return $this->assertion;
  }
  /**
   * @param Target
   */
  public function setCanonicalTarget(Target $canonicalTarget)
  {
    $this->canonicalTarget = $canonicalTarget;
  }
  /**
   * @return Target
   */
  public function getCanonicalTarget()
  {
    return $this->canonicalTarget;
  }
  /**
   * @param Declaration
   */
  public function setDeclaration(Declaration $declaration)
  {
    $this->declaration = $declaration;
  }
  /**
   * @return Declaration
   */
  public function getDeclaration()
  {
    return $this->declaration;
  }
  /**
   * @param string
   */
  public function setFilePath($filePath)
  {
    $this->filePath = $filePath;
  }
  /**
   * @return string
   */
  public function getFilePath()
  {
    return $this->filePath;
  }
  /**
   * @param string
   */
  public function setInternalMetadata($internalMetadata)
  {
    $this->internalMetadata = $internalMetadata;
  }
  /**
   * @return string
   */
  public function getInternalMetadata()
  {
    return $this->internalMetadata;
  }
  /**
   * @param Notebook
   */
  public function setNotebook(Notebook $notebook)
  {
    $this->notebook = $notebook;
  }
  /**
   * @return Notebook
   */
  public function getNotebook()
  {
    return $this->notebook;
  }
  /**
   * @param Operations
   */
  public function setOperations(Operations $operations)
  {
    $this->operations = $operations;
  }
  /**
   * @return Operations
   */
  public function getOperations()
  {
    return $this->operations;
  }
  /**
   * @param Relation
   */
  public function setRelation(Relation $relation)
  {
    $this->relation = $relation;
  }
  /**
   * @return Relation
   */
  public function getRelation()
  {
    return $this->relation;
  }
  /**
   * @param Target
   */
  public function setTarget(Target $target)
  {
    $this->target = $target;
  }
  /**
   * @return Target
   */
  public function getTarget()
  {
    return $this->target;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CompilationResultAction::class, 'Google_Service_Dataform_CompilationResultAction');
