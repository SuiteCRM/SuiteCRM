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

namespace Google\Service\BackupforGKE;

class TransformationRuleAction extends \Google\Model
{
  /**
   * @var string
   */
  public $fromPath;
  /**
   * @var string
   */
  public $op;
  /**
   * @var string
   */
  public $path;
  /**
   * @var string
   */
  public $value;

  /**
   * @param string
   */
  public function setFromPath($fromPath)
  {
    $this->fromPath = $fromPath;
  }
  /**
   * @return string
   */
  public function getFromPath()
  {
    return $this->fromPath;
  }
  /**
   * @param string
   */
  public function setOp($op)
  {
    $this->op = $op;
  }
  /**
   * @return string
   */
  public function getOp()
  {
    return $this->op;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TransformationRuleAction::class, 'Google_Service_BackupforGKE_TransformationRuleAction');
