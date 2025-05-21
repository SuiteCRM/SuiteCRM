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

namespace Google\Service\FirebaseAppDistribution;

class GoogleFirebaseAppdistroV1FeedbackReport extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $firebaseConsoleUri;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $screenshotUri;
  /**
   * @var string
   */
  public $tester;
  /**
   * @var string
   */
  public $text;

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
  public function setFirebaseConsoleUri($firebaseConsoleUri)
  {
    $this->firebaseConsoleUri = $firebaseConsoleUri;
  }
  /**
   * @return string
   */
  public function getFirebaseConsoleUri()
  {
    return $this->firebaseConsoleUri;
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
  public function setScreenshotUri($screenshotUri)
  {
    $this->screenshotUri = $screenshotUri;
  }
  /**
   * @return string
   */
  public function getScreenshotUri()
  {
    return $this->screenshotUri;
  }
  /**
   * @param string
   */
  public function setTester($tester)
  {
    $this->tester = $tester;
  }
  /**
   * @return string
   */
  public function getTester()
  {
    return $this->tester;
  }
  /**
   * @param string
   */
  public function setText($text)
  {
    $this->text = $text;
  }
  /**
   * @return string
   */
  public function getText()
  {
    return $this->text;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirebaseAppdistroV1FeedbackReport::class, 'Google_Service_FirebaseAppDistribution_GoogleFirebaseAppdistroV1FeedbackReport');
