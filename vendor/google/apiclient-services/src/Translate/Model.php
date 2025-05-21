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

namespace Google\Service\Translate;

class Model extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $dataset;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $sourceLanguageCode;
  /**
   * @var string
   */
  public $targetLanguageCode;
  /**
   * @var int
   */
  public $testExampleCount;
  /**
   * @var int
   */
  public $trainExampleCount;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var int
   */
  public $validateExampleCount;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDataset($dataset)
  {
    $this->dataset = $dataset;
  }
  /**
   * @return string
   */
  public function getDataset()
  {
    return $this->dataset;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setSourceLanguageCode($sourceLanguageCode)
  {
    $this->sourceLanguageCode = $sourceLanguageCode;
  }
  /**
   * @return string
   */
  public function getSourceLanguageCode()
  {
    return $this->sourceLanguageCode;
  }
  /**
   * @param string
   */
  public function setTargetLanguageCode($targetLanguageCode)
  {
    $this->targetLanguageCode = $targetLanguageCode;
  }
  /**
   * @return string
   */
  public function getTargetLanguageCode()
  {
    return $this->targetLanguageCode;
  }
  /**
   * @param int
   */
  public function setTestExampleCount($testExampleCount)
  {
    $this->testExampleCount = $testExampleCount;
  }
  /**
   * @return int
   */
  public function getTestExampleCount()
  {
    return $this->testExampleCount;
  }
  /**
   * @param int
   */
  public function setTrainExampleCount($trainExampleCount)
  {
    $this->trainExampleCount = $trainExampleCount;
  }
  /**
   * @return int
   */
  public function getTrainExampleCount()
  {
    return $this->trainExampleCount;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param int
   */
  public function setValidateExampleCount($validateExampleCount)
  {
    $this->validateExampleCount = $validateExampleCount;
  }
  /**
   * @return int
   */
  public function getValidateExampleCount()
  {
    return $this->validateExampleCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Model::class, 'Google_Service_Translate_Model');
