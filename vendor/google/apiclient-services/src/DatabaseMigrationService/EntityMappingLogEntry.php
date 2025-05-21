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

class EntityMappingLogEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $mappingComment;
  /**
   * @var string
   */
  public $ruleId;
  /**
   * @var string
   */
  public $ruleRevisionId;

  /**
   * @param string
   */
  public function setMappingComment($mappingComment)
  {
    $this->mappingComment = $mappingComment;
  }
  /**
   * @return string
   */
  public function getMappingComment()
  {
    return $this->mappingComment;
  }
  /**
   * @param string
   */
  public function setRuleId($ruleId)
  {
    $this->ruleId = $ruleId;
  }
  /**
   * @return string
   */
  public function getRuleId()
  {
    return $this->ruleId;
  }
  /**
   * @param string
   */
  public function setRuleRevisionId($ruleRevisionId)
  {
    $this->ruleRevisionId = $ruleRevisionId;
  }
  /**
   * @return string
   */
  public function getRuleRevisionId()
  {
    return $this->ruleRevisionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EntityMappingLogEntry::class, 'Google_Service_DatabaseMigrationService_EntityMappingLogEntry');
