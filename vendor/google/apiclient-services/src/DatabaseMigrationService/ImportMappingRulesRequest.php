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

namespace Google\Service\DatabaseMigrationService;

class ImportMappingRulesRequest extends \Google\Collection
{
  protected $collection_key = 'rulesFiles';
  /**
   * @var bool
   */
  public $autoCommit;
  protected $rulesFilesType = RulesFile::class;
  protected $rulesFilesDataType = 'array';
  /**
   * @var string
   */
  public $rulesFormat;

  /**
   * @param bool
   */
  public function setAutoCommit($autoCommit)
  {
    $this->autoCommit = $autoCommit;
  }
  /**
   * @return bool
   */
  public function getAutoCommit()
  {
    return $this->autoCommit;
  }
  /**
   * @param RulesFile[]
   */
  public function setRulesFiles($rulesFiles)
  {
    $this->rulesFiles = $rulesFiles;
  }
  /**
   * @return RulesFile[]
   */
  public function getRulesFiles()
  {
    return $this->rulesFiles;
  }
  /**
   * @param string
   */
  public function setRulesFormat($rulesFormat)
  {
    $this->rulesFormat = $rulesFormat;
  }
  /**
   * @return string
   */
  public function getRulesFormat()
  {
    return $this->rulesFormat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImportMappingRulesRequest::class, 'Google_Service_DatabaseMigrationService_ImportMappingRulesRequest');
