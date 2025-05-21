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

class CodeCompilationConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $assertionSchema;
  /**
   * @var string
   */
  public $databaseSuffix;
  /**
   * @var string
   */
  public $defaultDatabase;
  /**
   * @var string
   */
  public $defaultLocation;
  protected $defaultNotebookRuntimeOptionsType = NotebookRuntimeOptions::class;
  protected $defaultNotebookRuntimeOptionsDataType = '';
  /**
   * @var string
   */
  public $defaultSchema;
  /**
   * @var string
   */
  public $schemaSuffix;
  /**
   * @var string
   */
  public $tablePrefix;
  /**
   * @var string[]
   */
  public $vars;

  /**
   * @param string
   */
  public function setAssertionSchema($assertionSchema)
  {
    $this->assertionSchema = $assertionSchema;
  }
  /**
   * @return string
   */
  public function getAssertionSchema()
  {
    return $this->assertionSchema;
  }
  /**
   * @param string
   */
  public function setDatabaseSuffix($databaseSuffix)
  {
    $this->databaseSuffix = $databaseSuffix;
  }
  /**
   * @return string
   */
  public function getDatabaseSuffix()
  {
    return $this->databaseSuffix;
  }
  /**
   * @param string
   */
  public function setDefaultDatabase($defaultDatabase)
  {
    $this->defaultDatabase = $defaultDatabase;
  }
  /**
   * @return string
   */
  public function getDefaultDatabase()
  {
    return $this->defaultDatabase;
  }
  /**
   * @param string
   */
  public function setDefaultLocation($defaultLocation)
  {
    $this->defaultLocation = $defaultLocation;
  }
  /**
   * @return string
   */
  public function getDefaultLocation()
  {
    return $this->defaultLocation;
  }
  /**
   * @param NotebookRuntimeOptions
   */
  public function setDefaultNotebookRuntimeOptions(NotebookRuntimeOptions $defaultNotebookRuntimeOptions)
  {
    $this->defaultNotebookRuntimeOptions = $defaultNotebookRuntimeOptions;
  }
  /**
   * @return NotebookRuntimeOptions
   */
  public function getDefaultNotebookRuntimeOptions()
  {
    return $this->defaultNotebookRuntimeOptions;
  }
  /**
   * @param string
   */
  public function setDefaultSchema($defaultSchema)
  {
    $this->defaultSchema = $defaultSchema;
  }
  /**
   * @return string
   */
  public function getDefaultSchema()
  {
    return $this->defaultSchema;
  }
  /**
   * @param string
   */
  public function setSchemaSuffix($schemaSuffix)
  {
    $this->schemaSuffix = $schemaSuffix;
  }
  /**
   * @return string
   */
  public function getSchemaSuffix()
  {
    return $this->schemaSuffix;
  }
  /**
   * @param string
   */
  public function setTablePrefix($tablePrefix)
  {
    $this->tablePrefix = $tablePrefix;
  }
  /**
   * @return string
   */
  public function getTablePrefix()
  {
    return $this->tablePrefix;
  }
  /**
   * @param string[]
   */
  public function setVars($vars)
  {
    $this->vars = $vars;
  }
  /**
   * @return string[]
   */
  public function getVars()
  {
    return $this->vars;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CodeCompilationConfig::class, 'Google_Service_Dataform_CodeCompilationConfig');
