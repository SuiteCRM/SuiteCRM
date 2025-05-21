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

namespace Google\Service\Networkconnectivity;

class PscPropagationStatus extends \Google\Model
{
  /**
   * @var string
   */
  public $code;
  /**
   * @var string
   */
  public $message;
  /**
   * @var string
   */
  public $sourceForwardingRule;
  /**
   * @var string
   */
  public $sourceGroup;
  /**
   * @var string
   */
  public $sourceSpoke;
  /**
   * @var string
   */
  public $targetGroup;
  /**
   * @var string
   */
  public $targetSpoke;

  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
  /**
   * @param string
   */
  public function setSourceForwardingRule($sourceForwardingRule)
  {
    $this->sourceForwardingRule = $sourceForwardingRule;
  }
  /**
   * @return string
   */
  public function getSourceForwardingRule()
  {
    return $this->sourceForwardingRule;
  }
  /**
   * @param string
   */
  public function setSourceGroup($sourceGroup)
  {
    $this->sourceGroup = $sourceGroup;
  }
  /**
   * @return string
   */
  public function getSourceGroup()
  {
    return $this->sourceGroup;
  }
  /**
   * @param string
   */
  public function setSourceSpoke($sourceSpoke)
  {
    $this->sourceSpoke = $sourceSpoke;
  }
  /**
   * @return string
   */
  public function getSourceSpoke()
  {
    return $this->sourceSpoke;
  }
  /**
   * @param string
   */
  public function setTargetGroup($targetGroup)
  {
    $this->targetGroup = $targetGroup;
  }
  /**
   * @return string
   */
  public function getTargetGroup()
  {
    return $this->targetGroup;
  }
  /**
   * @param string
   */
  public function setTargetSpoke($targetSpoke)
  {
    $this->targetSpoke = $targetSpoke;
  }
  /**
   * @return string
   */
  public function getTargetSpoke()
  {
    return $this->targetSpoke;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscPropagationStatus::class, 'Google_Service_Networkconnectivity_PscPropagationStatus');
